{-

AIF Omega version 2016

Developed under BSD license as part of OFMC

(C) Copyright Alessandro Bruni 2015,2016
(C) Copyright DTU 2010,2016
(C) Copyright Paolo Modesti 2010
(C) Copyright IBM Corp. 2010

All Rights Reserved.

-}

module PrettyPrinter where

import Types
import FPASLan
import Data.List
import qualified Data.Map as Map

-- functions that are generic for all pretty printers

val_occurrences :: Symbtab -> [PRule] -> [PRule]
val_occurrences tab rules =
  nub
     [ (PRule { ruleName = "occurs",
                ruleEnv = [],
                lf = (PFact "timplies" [PAtom "X", PAtom "Y"]):[(subposf f p (PAtom "X"))],
                lp = [],
                ln = [],
                fresh = [],
                rf = [(subposf f p (PAtom "Y"))],
                rp = [] })
     | (f,poses) <- nub $ getOccrs tab rules, p<-poses ]

type Position = [Int]

getConsts rules =
  nub $ concatMap (\(PRule _ [] lf _ _ _ rf _) ->
    nub $ concatMap getConstsF (lf ++ rf)) rules

getConstsF (PFact f args) = nub $ concatMap getConstsT args

getConstsT (PComp id ts) = nub $ concatMap getConstsT ts
getConstsT (PAtom id) = [(id, 0) | isConst (PAtom id)]

getOccrs :: Symbtab -> [PRule] -> [(PFact,[Position])]
getOccrs tab = concatMap (getOccr tab)

getOccr :: Symbtab ->  PRule -> [(PFact,[Position])]
getOccr tab (PRule _ [] lf _ _ _ rf _) =
  [ (f,posVal tab f) | f<-lf++rf]

posVal :: Symbtab -> PFact -> [Position]
posVal tab (PFact "timplies" _) = []
posVal tab (PFact f ts) =
  nub [ (i:p) | (i,t)<- zip [1..] ts, p<- posValt tab [] t]

posValt :: Symbtab -> Position -> PTerm -> [Position]
posValt tab p (PAtom _) = []
posValt tab p (PComp "val" ts) = [p]
posValt tab p (PComp f ts) = concatMap (\ (i,t) -> posValt tab (p++[i]) t ) (zip [1..] ts)

subposf :: PFact -> Position -> PTerm -> PFact
subposf (PFact f ts) (i:p) t =
  (PFact f $ seti ts i (subpost (head $ drop (i-1) ts) p t))

subpost :: PTerm -> Position -> PTerm -> PTerm
subpost _ [] t' = t'
subpost (PComp f ts) (i:p) t' = PComp f (seti ts i (subpost (head $ drop (i-1) ts) p t'))
subpost t p t' = error $ (show t)++"\n"++(show p)++"\n"++(show t')

pX :: String -> (a->String) -> [a] -> String
pX sep f [] = ""
pX sep f [x] = f x
pX sep f xs = foldl (\ x y -> x++sep++(f y)) (f (head xs)) (tail xs)

{------- depricated pretty printer for SPASS

pASLan :: Symbtab -> Int -> String -> [PRule]  -> String
pASLan tab numsets name rules = --- (name,types,sets,pubfun,privfun,facts,rules) =
  let sym = (Map.toList tab) in
  "begin_problem("++name++").\n"++
  "list_of_descriptions.\n"++
  "name({*"++name++"*}).\n"++
  "author({*FPASLan-to-SPASS compiler*}).\n"++
  "status(unknown).\n"++
  "description({*N/A*}).\n"++
  "end_of_list.\n\n"++
  "list_of_symbols.\n"++
  "functions["++
  (pX "," pSym (("val",FunSym False numsets): [(name,f) | (name,f)<-sym, isFun f || isEnuCo f]))++
  "].\n"++
  "predicates["++
  (pX "," pSym ([(name,f) | (name,f)<-sym, isEnu f || isFact f]))++
  "].\n"++
  "end_of_list.\n\n"++
  "list_of_formulae(axioms).\n"++
  (pX "\n" pRule rules)++"\n"++
  "%occs \n"++
  (pX "\n" pRule (val_occurrences tab rules))++"\n"++
  "end_of_list.\n\n"++
  "list_of_formulae(conjectures).\n"++
  "formula(attack).\n"++
  "end_of_list.\n"++
  "end_problem.\n"


pRule :: PRule -> String
pRule (PRule _ [] lf _ _ _ rf _) =
  let vars = nub $ fvfs(lf)++fvfs(rf)
      formula = if null lf then pFacts rf else
                "implies("++(pFacts lf)++",\n"++
                (pFacts rf)++")"
      quantifier = if null vars then formula
                   else "forall(["++(pX "," (\x-> x) vars)++"],"++formula++")"
  in "formula("++quantifier++").\n"

pFacts [] = error "Empty RHS"
pFacts [x] = pFact x
pFacts (x:xs) = "and("++(pFact x)++","++(pFacts xs)++")"

pSym :: (PIdent,SymbEntry) -> String
pSym (f,FunSym _ n) = "("++f++","++(show n)++")"
pSym (c,EnuCo) = "("++c++",0)"
pSym (x,VarEnu _) = "("++x++",1)"
pSym (f,FactSym n) = "("++f++","++(show n)++")"

pTerms = pX "," pTerm

pTerm (PAtom id) = id
pTerm (PComp id ts) = id++"("++(pTerms ts)++")"

pFact (PFact id ts) =  if (length ts)==0 then id else id++"("++(pTerms ts)++")"
-}

------- pretty printer for SPASS

pASLan :: PASLan  -> String
pASLan p =
  "begin_problem("++name p++").\n"++
  "list_of_descriptions.\n"++
  "name({*"++name p++"*}).\n"++
  "author({*FPASLan-to-SPASS compiler*}).\n"++
  "status(unknown).\n"++
  "description({*N/A*}).\n"++
  "end_of_list.\n\n"++
  "list_of_symbols.\n"++
  "functions["++
  (intercalate ","
    $ map pSym $ ("val",length $ setdecs p):privfundec p++pubfundec p++getConsts (rules p))++
  "].\n"++
  "predicates["++
  (intercalate ","
    $ map pSym $ facdec p)++
  "].\n"++
  "end_of_list.\n\n"++
  "list_of_formulae(axioms).\n"++
  (intercalate "\n" $ map pRule $ rules p)++"\n"++
  --- occs should be taken care of earlier in the translation
  ---"%occs \n"++
  ---(pX "\n" pRule (val_occurrences tab rules))++"\n"++
  "end_of_list.\n\n"++
  "list_of_formulae(conjectures).\n"++
  "formula(attack).\n"++
  "end_of_list.\n"++
  "end_problem.\n"

pRule :: PRule -> String
pRule (PRule _ [] lf _ _ _ rf _) =
  let vars = nub $ fvfs(lf)++fvfs(rf)
      formula = if null lf then pFacts rf else
                "implies("++(pFacts lf)++",\n"++
                (pFacts rf)++")"
      quantifier = if null vars then formula
                   else "forall(["++(pX "," (\x-> x) vars)++"],"++formula++")"
  in "formula("++quantifier++").\n"

pFacts [] = error "Empty RHS"
pFacts [x] = pFact x
pFacts (x:xs) = "and("++(pFact x)++","++(pFacts xs)++")"

pSym :: (PIdent,Int) -> String
pSym (f,n) = "("++f++","++(show n)++")"

pTerms = pX "," pTerm

pTerm (PComp "0" []) = "zero"
pTerm (PAtom id) = id
pTerm (PComp id ts) = if null ts then id else id++"("++(pTerms ts)++")"

pFact (PFact id ts) =  if null ts then id else id++"("++(pTerms ts)++")"

{-
  "Types: "++(pX pTypedec types)++"\n"++
  "Sets: "++(pTerms sets)++"\n"++
  "Symbols: "++(pX pSymbol (pubfun++privfun++facts))++"\n"++
  "Rules: "++(pX pRule rules)++"\n"

pL = pX (\x -> x)

pTypedec (id,typ) = (pL id)++":"++(pTyp typ)++";"

pTyp (Enum list) = "{" ++ (pL list) ++ "}"
pTyp (Value) = "value"
pTyp (Untyped) = "untyped"

pSymbol (id,i) = id++"/"++(show i)

pRule :: PRule -> String
pRule (lambda,lf,ps,ns,f,rf,rs) =
  (if null lambda then "" else "\\"++(pL lambda)++".\n")++
  (if null lf then "" else (pX pFact lf)++"\n")++
  (if null ps then "" else (pX pCond ps)++"\n")++
  (if null ns then "" else (pX pNCond ns)++"\n")++
  "=["++(pL f)++"]=>\n"++
  (if null rf then "" else (pX pFact rf)++"\n")++
  (if null rs then "" else (pX pCond rs)++"\n")++"\n"
pCond ([],el,set) = el++" in "++(pTerm set)
pCond (al,el,set) = error "Allquantified positive condition"
pNCond ([],el,set) = el++" notin "++(pTerm set)
pNCond (al,el,set) = "forall "++(pL al)++"."++el++" notin "++(pTerm set)

-}


{- ------- depricated pretty printer for ProVerif
prASLan :: Symbtab -> Int -> String -> [PRule]  -> String
prASLan tab numsets name rules =
  let sym = (Map.toList tab) in
  "pred iknows/1 elimVar,decompData.\n"++
  (concatMap (\x -> "pred "++(prSym x)++".\n")
   ([(name,f) | (name,f)<-sym, isEnu f || isFact f, name/="iknows"]))++
  "nounif iknows:x.\n"++
  "fun nothing/0.\n"++
  (concatMap (\x -> "fun "++(prSym x)++".\n")
   (("val",FunSym False numsets):
    [(name,f) | (name,f)<-sym, isFun f, (FunSym _ n) <- [f], n>0]))++
  "query attack:.\n"++
  "reduc\n\n"++
  (pX "\n" prRule rules)++"\n"++
  "(* occs *) \n"++
  (pX "\n" prRule (val_occurrences tab rules))++"\n"++
  "iknows:nothing."

prRule :: PRule -> String
prRule (PRule _ [] lf _ _ _ rf _) =
  let vars = nub $ fvfs(lf)++fvfs(rf)
  in concat [ (if null lf then prFact r
               else (prFacts lf)++" -> "++(prFact r))++";\n"
            | r <- rf ]

prFacts [] = error "Empty RHS"
prFacts [x] = prFact x
prFacts (x:xs) = (prFact x)++" & "++(prFacts xs)

prSym :: (PIdent,SymbEntry) -> String
prSym (f,FunSym _ n) = f++"/"++(show n)
prSym (c,EnuCo) = error "Co no in pv"
prSym (x,VarEnu _) = x++"/1"
prSym (f,FactSym n) = f++"/"++(show n)


prTerms = pX "," prTerm

prTerm (PAtom "0") = "Num0[]"
prTerm (PAtom "1") = "Num1[]"
prTerm (PAtom id) = if (head id) `elem` ['a'..'z'] then "C"++id++"[]" else "v"++id
prTerm (PComp id ts) = id++"("++(prTerms ts)++")"

prFact (PFact id ts) =  id++":"++(prTerms ts)
-}


-- Pretty printer for ProVerif
pvASLan :: PASLan -> String
pvASLan p =
  "pred iknows/1 elimVar,decompData.\n"++
  concatMap (\x -> "pred "++pvSym x++".\n")
   [(name,f) | (name,f)<-facdec p, name/="iknows"]++
  "nounif iknows:x.\n"++
  "fun nothing/0.\n"++
  concatMap (\x -> "fun "++pvSym x++".\n")
   (("val", length (setdecs p)):privfundec p++pubfundec p)++
  "query attack:.\n"++
  "reduc\n\n"++
  intercalate ";\n\n" (map pvRule (rules p))++".\n"
  -- Occurs not yet implemented
  -- "(* occs *) \n"++
  -- (pX "\n" pvRule (Val_occurrences tab rules))++"\n"++


pvRule :: PRule -> String
pvRule (PRule name [] lf _ _ _ rf _) =
  let vars = nub $ fvfs lf++fvfs rf
  in "(* "++name++" *)\n"++intercalate ";\n"
     [if null lf then pvFact r
      else pvFacts lf++" -> "++pvFact r
     | r <- rf ]

pvFact (PFact id ts) =  id++":"++pvTerms ts

pvFacts [] = error "Empty RHS"
pvFacts [x] = pvFact x
pvFacts (x:xs) = pvFact x++" & "++pvFacts xs

pvSym :: (PIdent,Int) -> String
pvSym (f,n) = f++"/"++show n

pvTerms terms = intercalate "," (map pvTerm terms)

pvTerm (PComp "0" []) = "Num0[]"
pvTerm (PAtom id) = if isConst (PAtom id) then "C"++id++"[]" else "v"++id
pvTerm (PComp id ts) = id++"("++pvTerms ts++")"
