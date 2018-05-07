{-

AIF Omega version 2016

Developed under BSD license as part of OFMC

(C) Copyright Alessandro Bruni 2015,2016
(C) Copyright DTU 2010,2016
(C) Copyright Paolo Modesti 2010
(C) Copyright IBM Corp. 2010

All Rights Reserved.

-}

module Modesti where

import Types
import Data.List

-- <paolo>
-- definitions translation and "occurs"

mkPrepros :: PASLan -> PASLan
mkPrepros (PASLan name types sets pubfun privfun facts definitions rules) = let 
                                                                                                         definitions1 = mkDefinitions definitions definitions                           
                                                                                                    in (PASLan name types sets pubfun privfun (mkFacts facts) definitions1 (mkOccurs . deParRules . mkDefinitions rules $ definitions1)) 

-- "occurs"

occursfun :: String
occursfun = "occurs"

mkFacts :: [(PIdent,Int)] -> [(PIdent,Int)]
mkFacts x = x ++ [(occursfun,1)]

occurs :: PIdent -> PFact
occurs idf = (PFact occursfun [(PAtom idf)])

mkOccurs :: [PRule]->[PRule]
mkOccurs xs = map mkOccur xs

mkOccur :: PRule -> PRule
mkOccur (PRule nm lamvar lf ps ns f rf rs) = let 
                                                        -- ident in term in the left side
                                                        lf1 = lf ++ (map (\x -> occurs x)) (nub ((getCondIds ps)) \\ f)
                                                        -- fresh terms
                                                        rf1 = rf ++ (map (\x -> occurs x)) (nub f)
                                                    in (PRule nm lamvar lf1 ps ns f rf1 rs) 

getCondIds :: [PCond] -> [PIdent]
getCondIds [] =[]
getCondIds ((PCond [] id _):[]) = [id]
getCondIds (x:xs) = getCondIds [x] ++ getCondIds xs

-- definitions translation

mkDefinitions :: Def a => [a] -> PDefinitions -> [a]
mkDefinitions a [] = a
mkDefinitions [] _ = []
mkDefinitions a defs = map (\x -> mkDefs(x) defs) a

mkDefs :: Def a => a -> PDefinitions -> a     
mkDefs a [] = a
mkDefs a (def:[]) = mkDef [def] a
mkDefs a (def:defs) = mkDef [def] (mkDefs a defs)

class Def a where
        mkDef :: PDefinitions -> a -> a

instance Def PRule where
        mkDef def (PRule nm lamvar lf ps ns f rf rs) = (PRule nm lamvar (mkDef def lf) ps ns f (mkDef def rf) rs)

instance Def [PRule] where
        mkDef def xs = map (mkDef def) xs
        
instance Def [PTerm] where
        mkDef def xs = map (mkDef def) xs

instance Def [PDefinition] where
        mkDef _ [] = []
        mkDef def (x:[]) = [mkDef def x]
        mkDef def (x:xs) = mkDef def (mkDef def x):xs 

instance Def PDefinition where
        mkDef def1 (idf,def,pars) = (idf,mkDef def1 def,pars)

instance Def [PFact] where
        mkDef def xs = map (mkDef def) xs

instance Def [PCond] where
        mkDef def xs = map (mkDef def) xs
                
instance Def PFact where
        mkDef def (PFact ident terms) = (PFact ident (mkDef def terms)) 
        
instance Def PCond where
        mkDef def (PCond idents ident term) = (PCond idents ident (mkDef def term))

instance Def PTerm where
        mkDef ((idf,def,_):[]) (PAtom ident) = if ident==idf then def else PAtom ident
        mkDef _ (PComp ident []) = PComp ident []
        mkDef (def@(idf,defs,xds):[]) (PComp ident xs) = if ident==idf && length(xds)==length(xs) then trDefPar defs xds xs else PComp ident (mkDef [def] xs)
        mkDef ((idf,defs,xds):[])  (PPar ident xps) = if ident==idf && length(xds)==length(xps) then trDefPar defs xds xps else PPar ident xps
        mkDef _ (PParId xps) = PParId xps
        mkDef def term = error(show def ++ " definition not applicable on " ++ show term)

deParRule :: PRule -> PRule
deParRule (PRule nm lamvar lf ps ns f rf rs) = (PRule nm lamvar (deParFacts lf) ps ns f (deParFacts rf) rs)

deParRules :: [PRule] -> [PRule]
deParRules xs = map deParRule xs  

deParFacts :: [PFact] -> [PFact]
deParFacts xs = map deParFact xs 

deParFact :: PFact -> PFact
deParFact f@(PFact _ []) = f
deParFact (PFact ident terms) = (PFact ident (dePars terms))

dePar :: PTerm -> PTerm
dePar t@(PAtom _) = t
dePar t@(PComp _ []) = t
dePar (PParId idf) = PAtom idf
dePar (PComp ident (x:xs)) = PComp ident (dePar x : dePars xs)

dePar (PPar ident pars) = PComp ident pars

dePars :: [PTerm] -> [PTerm]
dePars xs = map dePar xs

trDefPars :: [PTerm] -> [PIdent] -> [PTerm] -> [PTerm]
trDefPars xs xds xps = map (\x -> trDefPar x xds xps) xs

trDefPar :: PTerm -> [PIdent] -> [PTerm] -> PTerm
trDefPar (PPar ident ids) xds xps = PPar ident [snd x | x <-zip xds xps, elem (PAtom (fst x)) ids]
trDefPar (PComp ident (x:xs)) xds xps = PComp ident ([trDefPar x xds xps]++trDefPars xs xds xps)
trDefPar (PParId ident) xds xps = if (elem ident xds) then (head [snd x | x <-zip xds xps, elem (fst x) [ident]]) else PParId ident
trDefPar (PAtom ident) _ _  = PParId ident
trDefPar t _ _ = t

--------------- debug -----------------------------
dbgDefs  :: PASLan -> String
dbgDefs (paslan@(PASLan name types sets pubfun privfun facts definitions rules)) = 
        let 
             str = "\n\n--- Definitions ---\n\n" ++ printDefs definitions ++ "\n--- Rules ---\n\n" ++ printRules rules
             p1@(PASLan name types sets pubfun privfun facts definitions1 r1) = mkPrepros paslan
             str1 = "\n--- Definitions ---\n\n" ++ printDefs definitions1 ++ "\n--- Rules Changed ---\n\n" ++ printRules r1 ++ "\n--- Protocol Translation ---\n\n"
         in  (str ++ str1)
              
printDefs :: PDefinitions -> String
printDefs ([]) = show ""
printDefs (x:[]) = printDef x
printDefs (x:xs) = printDef x ++ printDefs xs

printDef :: PDefinition -> String
printDef (idf,def,pars) = show idf ++ "(" ++ show pars ++ "): " ++ show def ++ "\n"

printRules :: [PRule] -> String
printRules ([]) = show ""
printRules (x:[]) = printRule x
printRules (x:xs) = printRule x ++ printRules xs
        
printRule :: PRule -> String
printRule rule = show rule ++ "\n"

--------------------------------------------------

-- </paolo>
