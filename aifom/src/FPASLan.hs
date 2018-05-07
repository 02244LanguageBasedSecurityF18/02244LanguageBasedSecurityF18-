{-

Open Source Fixedpoint Model-Checker version 2010

(C) Copyright IBM Corp. 2010

All Rights Reserved.

Modifications by Paolo Modesti 2010

-}

module FPASLan
where
import FPASLanLexer
import FPASLanParser
import Types
import Data.Char
import Data.List
import Data.Maybe
import qualified Data.Map as Map

debug = False
spassout = True

-- Constructs the symbol table from a PASLan problem
mkSymbtab :: PASLan -> Symbtab
mkSymbtab p =
  (mkSTfac (facdec p) .
   mkSTfun (pubfundec p) (privfundec p) .
   mkSTsets (setdecs p))
  (mkSTtypes (typedec p))

builtinSym =
 [("0",FunSym True 0),
  ("1",FunSym True 0),
  ("timplies",FactSym 2),
  --- ("val",FunSym False 0),
  ("epsilon",FunSym True 0)
  --("occurs",FactSym 1)
 ]

mkSTtypes = foldr mkSTtype (Map.fromList builtinSym)

mkSTtype (var,Enum ids) table =
  let checkEC key old new
        | isVar key = error $ key ++ " is a variable (used in enum)"
        | new == EnuCo = EnuCo
        | otherwise =
          error $ key ++ " used in enumeration and in some other way"
      tab' = foldr (\ id tab -> Map.insertWithKey checkEC id EnuCo tab) table ids
      ty   = if isVar var then VarEnu ids
             else error $ var ++ " is not a variable name"
  in insertOnce var ty tab'

mkSTtype (var,Value) table =
  insertOnce var (if isVar var then VarVal else ConVal) table
mkSTtype (var,Untyped) table =
  insertOnce var VarUntyped table

mkSTsets sets table = foldr mkSTset table sets

mkSTset :: PTerm -> Symbtab -> Symbtab
mkSTset (PComp setname args) table =
  let checkname n = let r = Map.lookup n table
                    in if isJust r then
                         if isEnu $ fromJust r then n
                         else error $ n ++ " is not an enumeration type"
                       else error $ n ++ " is undefined "
      getname (PAtom m) = checkname m
      getname m = error $ show m ++ " as argument in set description"
  in if isVar setname then error $ setname ++ " is not a setname"
     else insertOnce setname (SetSym (map getname args)) table
mkSTset term _ = error $ show term ++ " is not a set description"

mkSTfun pub priv =
  insertMultiple (
    map (\ (f,n) -> if isVar f then error $ f++" is not a function symbol"
                    else (f,FunSym True n)) pub ++
    map (\ (f,n) -> if isVar f then error $ f++" is not a function symbol"
                    else (f,FunSym False n)) priv)

mkSTfac facts =
  insertMultiple
  (map (\ (f,n) -> if isVar f then error $ f++" is not a fact symbol"
                   else (f,FactSym n)) facts)

------------------------------

chexpl :: String -> Bool -> Bool
chexpl s b = if b then b else error s

checkrules :: Symbtab -> [PRule] -> Bool
checkrules tab =
  all (checkrule tab)

checkrule tab (PRule _ lambda lf ps ns f rf rs) =
  let defs0 = nub $ fvfs lf ++ fvss ps
      defs  = nub $ map (typeId.snd) lambda ++ f ++ filter (not . isEnu . (!) tab) defs0
      uses  = nub $ fvfs rf ++ fvss rs ++ fvss ns ++ filter (isEnu . (!) tab) defs0 in
  all (checkDefEnu tab) (map (typeId.snd) lambda) &&
  all (checkFact tab) lf &&
  all (checkFact tab) rf &&
  all (checkCond tab True ) ps &&
  all (checkCond tab False) ns &&
  all (checkCond tab True ) rs &&
  chexpl ("using undefined vars: " ++ show (uses \\ defs)) (null (uses \\ defs)) &&
  chexpl "creating defined var" (null (defs0 `intersect` nub f))

m ! k = fromMaybe (error $ k++" undefined") $ Map.lookup k m

checkFact :: Symbtab  -> PFact -> Bool
checkFact tab (PFact f ts) =
  case tab ! f of
  (FactSym n) -> chexpl ("arity of "++f) $ n==length ts && all (checkTerm tab) ts
  _ -> error $ f++" is not a fact symbol."

checkTerm tab (PAtom a) =
  case tab ! a of
  (FunSym _ _) -> error $ "Illegal atom "++a
  (FactSym _)  -> error $ "Illegal atom "++a
  (SetSym _) -> error $ "Illegal atom "++a
  _ -> True

checkTerm tab (PComp f ts) =
  let (FunSym _ n) = tab ! f
  in chexpl ("arity of "++f) $ n==length ts && all (checkTerm tab) ts

-- definitions --
checkTerm tab (PParId a) = error $ "error in definitions translation - Illegal PParId "++a
checkTerm tab (PPar f ts) = error $ "error in definitions translation - Illegal PPar "++f
--

checkCond tab positive (PCond uni t s) =
  (if positive then chexpl "positive fact with quantifier" $ null uni
   else all (checkDefEnu tab) uni) &&
  checkDefVal tab t &&
  checkSet tab s

checkDefEnu tab v = chexpl (v++" is not an enumeration constant") $ isEnu (tab ! v)
checkDefVal tab t = chexpl (t++" is not a value") $ isVal (tab ! t)
checkDefSet tab s = chexpl (s++" is not a set constructor") $ isSet (tab ! s)

checkSet :: Symbtab -> PTerm -> Bool
checkSet tab (PAtom a) = error $ a++" is not a set."
checkSet tab (PComp f ts) =
  let (SetSym args) = tab ! f
      ts' = map getIdent ts
      getIdent (PAtom a) = a
      getIdent a = error $ show a ++ " is not a valid argument for a set term"
  in chexpl ("wrong length of arguments "++show ts) $ length args == length ts &&
     all (checkArg tab) (zip ts' args)

checkSet tab (PParId a) = error $ a++" is not a set."
checkSet tab (PPar f ts) = error $ f++" is not a valid argument"

checkArg :: Symbtab -> (PIdent,PIdent) -> Bool
checkArg tab (actual,formal) =
  let (VarEnu fextension) = tab ! formal
  in case tab ! actual of
     EnuCo -> chexpl (actual++" not in "++formal) $ actual `elem` fextension
     VarEnu m -> chexpl ("sets with too large extension "++actual) $ null (m\\fextension)
     _ -> error $ "illegal parameter in set term: "++actual

------- quantifier elimination
-- In AIF quantifiers were just syntactic sugar for multiple rules,
-- while AIF-omega introduces variables as parameters to rules. Hence
-- we distinguish between finite and countable enumerations: the
-- former get compiled away, whilst the latter are kept as parameters.

quantelims :: Symbtab -> [PRule] -> [PRule]
quantelims tab = concatMap $ quantelim tab

quantelim :: Symbtab -> PRule -> [PRule]
quantelim tab (rule@(PRule name lamvar lf ps ns f rf rs)) =
  let nsqf = quantelimns tab ns in
  foldr ((\ var rules ->
           concat [subrs sigma rules | sigma <- getvals tab var ]) . typeId . snd)
         [PRule { ruleName = name, ruleEnv = [],
                   lf = lf, lp = ps, ln = nsqf,
                   fresh = f, rf = rf, rp = rs }] lamvar

typeId :: PType -> PIdent
typeId (VarOf x) = x

getvals :: Symbtab -> PIdent -> [Substitution]
getvals tab var =
  case tab ! var of
    VarEnu vals -> [Map.singleton var (PAtom v) |v<-vals]

quantelimns :: Symbtab -> [PCond] -> [PCond]
quantelimns tab = concatMap $ quantelimn tab

quantelimn :: Symbtab -> PCond -> [PCond]
quantelimn tab (PCond qs t s) =
  nub $
  foldr (\ var conds ->
           concat [subcs sigma conds | sigma <- getvals tab var])
        [PCond [] t s] qs

------- abstract

type Alpha = (PTerm -> Int)

mkAbs :: Symbtab -> [PTerm] -> (Alpha,Int)
mkAbs tab terms =
  let (allsets::[PTerm])
              = nub $
                foldr (\ var ts ->
                         concat [subts sigma ts| sigma<- getvals tab var])
                terms (fvts terms)
  -- ERROR in SPASS OUTPUT in some cases (val,num) where num = length allsets
  -- (val,0) returns 0
  in ((Map.!) (Map.fromList (zip allsets [1..])),length allsets)

absRules :: Symbtab -> [PRule] -> ([PRule],Int)
absRules tab rules =
  let ali = mkAbs tab (getSetsrs rules)
  in  (map (\rule -> foldr (absRule tab ali) rule (absr tab rule)) rules,snd ali)

absRule :: Symbtab -> (Alpha,Int) -> PIdent -> PRule -> PRule
absRule tab (alpha,numsets) abs (PRule nm [] lf ps ns f rf rs) =
  let fa (PCond [] t set) = t==abs
      ps'   = filter fa ps
      ns'   = filter fa ns
      rs'   = filter fa rs
      labs1 = ["ABS" ++ abs ++ show i| i<-[1..numsets]]
      labs2 = foldr (\(PCond [] t set) la  -> seti la (alpha set) "1") labs1 ps'
      labs  = foldr (\(PCond [] t set) la  -> seti la (alpha set) "0") labs2 ns'
      rabs1 = if abs `elem` f then ["0"| i <-[1..numsets]]
              else ["ABS" ++ abs ++ show i| i<-[1..numsets]]
      rabs2 = foldr (\(PCond [] t set) ra -> seti ra (alpha set) "0") rabs1 (ps'++ns')
      rabs  = foldr (\(PCond [] t set) ra -> seti ra (alpha set) "1") rabs2 rs'
      lval  = PComp "val" (map PAtom labs)
      rval  = PComp "val" (map PAtom rabs)
      lf'   = subfs (Map.singleton abs lval) lf
      rf'   = subfs (Map.singleton abs rval) rf
      tir   = if lval==rval || abs `elem` f then [] else [PFact "timplies" [lval,rval]]
  in PRule nm [] lf' ps ns f (tir++rf') rs

absrs tab = concatMap $ absr tab
absr :: Symbtab -> PRule -> [PIdent]
absr  tab (PRule _ [] lf ps ns f rf rs) =
  nub $
  absfs tab lf ++
  abscs tab ps ++
  abscs tab ns ++
  f ++
  absfs tab rf ++
  abscs tab rs
absfs tab = concatMap $ absf tab
absf  tab (PFact f ts) = absts tab ts
abscs tab = concatMap $ absc tab
absc  tab (PCond [] t set) = [t]
absts tab = concatMap $ abst tab
abst :: Symbtab -> PTerm -> [PIdent]
abst  tab (PAtom a) = [a | isVal (tab ! a)]
abst  tab (PComp f ts) = absts tab ts
abst  tab (PParId a) = error ("error in PParId " ++ a)

getSetsrs :: [PRule] -> [PTerm]
getSetsrs = concatMap getSetsr
getSetsr (PRule _ _ _ ps ns _ _ rs) =
  getSetscs ps ++ getSetscs ns ++ getSetscs rs
getSetscs = map (\ (PCond [] t set) -> set)

setMatch :: [PFact] -> [PFact]  -> [Substitution]
setMatch = setMatch0 Map.empty []

setMatch0 :: Substitution -> [PFact] -> [PFact] -> [PFact] -> [Substitution]
setMatch0 s _ [] facts = [s]
setMatch0 s hold (l:ls) (f:fs) = concat [setMatch0 s' [] (subfs s' ls) (fs++hold) | s' <- setMatchFact s l f]
                                 ++ setMatch0 s (f:hold) (l:ls) fs
setMatch0 _ _ _ _ = []

setMatchFact :: Substitution -> PFact -> PFact -> [Substitution]
setMatchFact s (PFact p xs) (PFact q ys) = if p/=q then [] else
                               setMatchTerms s xs ys

setMatchTerms :: Substitution -> [PTerm] -> [PTerm] -> [Substitution]
setMatchTerms s [] [] = [s]
setMatchTerms s (x:xs) (y:ys) = concat [ setMatchTerms s' (subts s' xs) ys | s' <- setMatchTerm s x y ]

setMatchTerm :: Substitution -> PTerm -> PTerm -> [Substitution]
setMatchTerm s (PAtom a) (PAtom b)
  | isVar a = [Map.insert a (PAtom b) s]
  | a == b = [s]
  | otherwise = []
setMatchTerm s (PAtom a) (PComp "val" bs) = [Map.insert a (PComp "val" bs) s | isVar a]
setMatchTerm s (PComp f ts) (PComp g ss) = if f/=g then [] else
                                           setMatchTerms s ts ss
setMatchTerm s _ _ = []



{-
subf  sub (f,ts) = (f,subts sub ts)
subcs sub = map (subc sub)
subc  sub ([],t,set) = ([],t,subt sub set)
subts sub = map (subt sub)
subt  sub (PAtom a) = Map.findWithDefault (PAtom a) a sub
subt  sub (PComp f ts) = PComp f (subts sub ts)
-}
