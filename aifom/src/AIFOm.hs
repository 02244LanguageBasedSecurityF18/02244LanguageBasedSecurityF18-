{-

AIF Omega version 2016

Developed under BSD license as part of OFMC

(C) Copyright Alessandro Bruni 2015-2017
(C) Copyright DTU 2010,2016
(C) Copyright Paolo Modesti 2010
(C) Copyright IBM Corp. 2010

All Rights Reserved.

-}

{-
TODO:
- implement uniqueness labels on set declarations
- finish the normalization phase
- implement uniqueness checks
- implement general sanity checks
-}

module AIFOm where

import Types
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Debug.Trace
import qualified Data.Map as Map
import qualified Data.Set as Set

getEnvTuple env k = fromMaybe
  (error $ "Identifier " ++ k ++ " not found in " ++ show env)
  (find (\ (k', _) -> k == k') env)

getEnv env k = v where (_, v) = getEnvTuple env k

getType typeEnv x =
  case getEnv typeEnv x of
    (VarOf id) -> id
    _ -> error $ "Identifier " ++ x ++ " is not a variable"

getSetDec p s =  fromMaybe (error $ "Set " ++ s ++ " not declared")
    (find
       (\ t ->
          case t of
              PComp s' _ -> s == s'
              _ -> False)
       (setdecs p))

isUserType ty =
  case ty of EnumType _ -> True; CountType -> True; Union args -> True; _ -> False

isFiniteTypeT typeEnv ty =
  case ty of
    EnumType _ -> True
    CountType -> False
    Union ids -> all (isFiniteType typeEnv) ids
    VarOf id -> isFiniteType typeEnv id
    Value -> False
    Untyped -> False
    _ -> error $ "AIFOm.isFiniteType: not a user defined type: " ++ show ty
isFiniteType typeEnv ty =
  isFiniteTypeT typeEnv $ getEnv typeEnv ty

isSubtypeOf typeEnv t1 t2 =
  if isVar t1 then
  typeOccurs t1 t2 ||
  case getEnv typeEnv t1 of
    EnumType range1 -> null $ subRange (nub range1) t2
    CountType -> False
    Union ids -> all (\t'-> isSubtypeOf typeEnv t' t2) ids
    VarOf t -> isSubtypeOf typeEnv t t2
    _ -> error $ "AIFOm.isSubtypeOf: not a user defined type: " ++ show t1
  else PAtom t1 `elem` getFiniteRange typeEnv t2
  where
    typeOccurs t1 t2 =
      t1 == t2 ||
      case getEnv typeEnv t2 of
        EnumType _ -> False
        CountType -> False
        Union ids -> any (typeOccurs t1) ids
        _ -> error $ "AIFOm.isSubtypeOf: not a user defined type: " ++ show t2
    subRange range1 t =
      case getEnv typeEnv t of
        EnumType range2 -> range1 \\ range2
        CountType -> range1
        Union ids -> foldl subRange range1 ids
        _ -> error $ "AIFOm.isSubtypeOf: not a user defined type: " ++ show t

getRangeTF f typeEnv ty =
  case ty of
    EnumType range -> map PAtom range
    CountType -> if f then error "AIFOm.getRange: not applicable to infinite types" else []
    Union ids -> nub $ concatMap (getRangeF f typeEnv) ids
    VarOf id -> getRangeF f typeEnv id
    _ -> error "AIFOm.getRange: not a user defined type"
getRangeF f typeEnv ty =
  getRangeTF f typeEnv (getEnv typeEnv ty)
getRangeT = getRangeTF True
getRange = getRangeF True
getFiniteRange = getRangeF False

allComb :: [[a]] -> [[a]]
allComb [] = []
allComb [x] = [[y] | y <- x]
allComb (x:xs) = [y:ys | y <- x, ys <- allComb xs]

-- Remove global variables
-- Remove finite enumerations
normalize :: PASLan -> PASLan
normalize p = fromJust (do
    tp <- typecheck p;
    return tp { setdecs = setDecsNew, facdec = factsNew, rules = rulesNew, typedec = userTypesNew })
  where
    -- Phase 1: filter user defined types from global variable
    -- declarations, introduce anonymous types for the latter
    isEnumType (ids, ty) = case ty of EnumType _ -> True; _ -> False
    (userTypes, globalVars) = partition (isUserType . snd) (typedec p)
    anonTypeName id = id ++ "__AnonType"
    anonTypes = concatMap (\ (id, ty) ->
      trace ("Deprecated: global variable `" ++ id ++ "` with anonymous type " ++ show ty) $
       case ty of
        Enum names -> [(anonTypeName id, EnumType names)]
        _ -> []) globalVars
    globalEnv :: [(PIdent, PType)] = concatMap (\ (id, ty) ->
      case ty of
        Enum names -> [(id, VarOf (anonTypeName id))]
        VarOf t -> [(id, VarOf t)]
        Value -> [(id, Value)]
        Untyped -> [(id, Untyped)]
        _ -> error "AIFOm.normalize: expecting variable declaration" ) globalVars
    userTypesNew = userTypes ++ anonTypes
    finiteUserTypes = [(ty, getRange userTypesNew ty) | (ty, _) <- userTypesNew, isFiniteType userTypesNew ty]
    -- Phase 1a: typecheck problem
    -- Phase 2: desugar set declarations
    setDecsNew = concatMap (\ setdec ->
      case setdec of
        PComp name [] -> [setdec]
        PComp name args ->
          if null desugaredArgs then [PComp name args] else
            [PComp (name ++ "__" ++ intercalate "__" (map atomName range)) infiniteArgs
            | range <- desugaredArgs]
          where
            (finiteArgs, infiniteArgs) = partition (isFiniteType userTypesNew . atomName) args
            desugaredArgs = allComb (map (getRange userTypesNew . atomName) finiteArgs)) (setdecs p)
    rulesNew = normalizeRules (rules p)
    -- Add standard facts
    factsNew = nub $ [("iknows",1),("attack",0),("occurs",1)] ++ facdec p
    normalizeRules = concatMap normalizeRule
    normalizeRule rule = multipleFamilyRules
      where
        -- All free variables must be declared globally, are now attached to the
        -- local environment
        freeVars = map (getEnvTuple globalEnv) (fvr rule)
        (finiteArgs, infiniteArgs) = partition (isFiniteTypeT userTypesNew . snd) (ruleEnv rule ++ freeVars)
        desugaredArgs = if null finiteArgs then [Map.empty]
          else map (Map.fromList . zip (map fst finiteArgs)) (allComb (map (getRangeT userTypesNew.snd) finiteArgs))
        desugaredRules = [newRule {lp = trConds (lp newRule), ln = trConds (ln newRule), rp = trConds (rp newRule)}|
                          rho <- desugaredArgs, let newRule = subr rho rule]
        -- Remove universal quantifiers with finite range on negative set
        -- conditions
        trCond (PCond allQ var (PComp set args)) =
          [PCond infiniteAllQ var (PComp newSet newArgs) | newSet <- newSets]
          where
            PComp _ setArgTypes = getSetDec p set
            (finiteSetArgs, infiniteSetArgs) = partition (isFiniteType userTypesNew . snd) (zip args (map atomName setArgTypes))
            (finiteAllQ, infiniteAllQ) = partition (isFiniteType userTypesNew) allQ
            desugaredSetArgs = allComb (map (\(x,y)->
              if isConst x then [x]
              else getRange userTypesNew y) finiteSetArgs)
            newSets = if null desugaredSetArgs then [set] else
              [set ++ "__" ++ intercalate "__" (map atomName setArgs) | setArgs <- desugaredSetArgs]
            newArgs = map fst infiniteSetArgs
        trConds = concatMap trCond
        -- Instantiate finite-range rule parameters
        instantiatedRules = concatMap instantiateRule desugaredRules
        instantiateRule r = [subr sigma r | sigma <- sigmas]
          where
            genSubsts ((var,VarOf ty):args) =
              if isFiniteType userTypesNew ty then
                [Map.insert var y sigma | y <- getRange userTypesNew ty, sigma <- genSubsts args]
              else genSubsts args
            genSubsts (_:args) = genSubsts args
            genSubsts [] = [Map.empty]
            sigmas = genSubsts (ruleEnv r)
        -- Apply the occurs check
        occursCheck r =
          r { lf = lf r ++ [PFact "occurs" [PAtom x] | x <- nf],
              rf = rf r ++ [PFact "occurs" [PAtom x] | x <- f] }
          where
            f = fresh r
            nf = map fst (filter (\(x,y)->y==Value) $ ruleEnv r) \\ f
        occursCheckRules = map occursCheck instantiatedRules
        multipleFamilies (PRule rname renv rlf rlp rln rfr rrf rrp) =
          buildNegRule dubiousMemberships : buildPosRules dubiousMemberships
          where dubiousMemberships = [ (var, set, length args) |
                  PCond [] var (PComp set args) <- rrp,
                  not (null args),
                  var `notElem` rfr,
                  isNothing (find (\(PCond [] var' (PComp set' args')) -> var'==var && set' == set) rlp),
                  isNothing (find (\(PCond _ var' (PComp set' args')) -> var'==var && set' == set) rln) ]
                buildNegRule [] = PRule rname renv rlf rlp rln rfr rrf rrp
                buildNegRule ((var, set, n):membs) =
                  PRule rname renv rlf rlp (PCond [] var (PComp set (replicate n (PAtom "_"))):rln) rfr rrf rrp
                  where PRule rname renv rlf rlp rln rfr rrf rrp = buildNegRule membs
                buildPosRules membs =
                  [PRule (rname++"__multifamily") (zip args argTypes++renv) rlf (PCond [] var (PComp set (map PAtom args)):rlp) rln [] [PFact "attack" []] []
                  | (var, set, n) <- membs, let args = [var ++ "__" ++ set ++ "__" ++ show i | i <- [1..n]], let argTypes = map (VarOf . atomName) $ compArgs (fromJust $ find (\x->set==compName x) setDecsNew)]
        multipleFamilyRules = concatMap multipleFamilies occursCheckRules

typecheck p = do
    checkTypeDecs (typedec p);
    checkSetDecs (setdecs p);
    checkUniques "Function" (pubfundec p ++ privfundec p);
    checkUniques "Fact" (facdec p);
    checkRules (rules p);
    return p
  where
    checkTypeDecs tds = do
      checkUniques "Type or global variable" tds;
      let True = all (checkTypeDec []) tds in Just ()
      where
        checkTypeDec :: [PIdent] -> Typedec -> Bool
        checkTypeDec visited (x, Union ys) =
          if x `elem` visited then error ("Type \"" ++ x ++ "\" is recursively defined")
          else all (checkTypeDec (x:visited)) (filter (\(y, _) -> y `elem` ys) tds)
        checkTypeDec _ (_, _) = True
    checkSetDecs (PComp s args:sds) = do
      if all (\(PComp s' _)-> s /= s') sds then Just ()
      else error ("Set \"" ++ s ++ "\" defined multiple times");
      checkUserTypes (map atomName args);
      return ()
      where
        checkUserTypes [] = return ()
        checkUserTypes (x:args) =
          case lookup x (typedec p) of
            Just t -> if isUserType t then checkUserTypes args
                      else error ("\"" ++ x ++ "\" is of kind variable")
            Nothing -> error ("Type \"" ++ x ++ "\" not defined")
    checkUniques category ((x, _):tds) =
      case lookup x tds of
        Just y -> error (category ++ " \"" ++ x ++ "\" defined multiple times")
        Nothing -> checkUniques category tds
    checkUniques _ [] = return ()
    checkRules [] = return ()
    checkRules (r:rs) = do
      checkRule r;
      checkRules rs
    checkRule (PRule rname renv lf lp ln fr rf rp) = do
        checkUniques localEnv
        checkAll checkFact (lf ++ rf)
        checkAll checkPosSetCond (lp ++ rp)
        checkAll checkNegSetCond ln
        return ()
      where
        localEnv = typedec p ++ renv
        checkUniques ((x, _):tds) =
          case lookup x tds of
            Just y -> error ("\"" ++ x ++ "\" defined multiple times in rule \"" ++ rname ++ "\" (including global environment)")
            Nothing -> checkUniques tds
        checkUniques [] = return ()
        checkAll f [] = return ()
        checkAll f (x:xs) = do
          f x;
          checkAll f xs
        checkFact (PFact f args) =
          case lookup f (facdec p) of
            Just n ->
              if length args /= n then error ("Fact \"" ++ f ++ "\" used with arity " ++ show (length args) ++ " instead of " ++ show n ++ " in rule \"" ++ rname ++ "\"")
              else checkTerms args
            Nothing -> error ("Fact \"" ++ f ++ "\" not declared")
        checkTerms [] = return ()
        checkTerms (t:ts) = do
          checkTerm t;
          checkTerms ts
        checkTerm (PAtom x) =
          if isConst (PAtom x) then return () -- TODO: check that the constant is defined
          else case lookup x localEnv of
            Just t -> when (isUserType t) $ error ("Name \"" ++ x ++ "\" is a user-defined type and not a variable (in rule " ++ rname ++ ")")
            Nothing -> error ("Variable \"" ++ x ++ "\" is not declared (in rule " ++ rname ++ ")")
        checkTerm (PComp f args) =
          case lookup f (pubfundec p ++ privfundec p) of
            Just n ->
              if length args /= n then error ("Function \"" ++ f ++ "\" used with arity " ++ show (length args) ++ " instead of " ++ show n ++ " in rule \"" ++ rname ++ "\"")
              else checkTerms args
            Nothing -> error ("Function \"" ++ f ++ "\" not declared")
        checkTerm _ = error "Macros not supported in AIF-omega"
        checkPosSetCond (PCond allQ var (PComp set args))
          | not (null allQ) = error $ "Universally quantified variables in positive set condition for the set " ++ set ++ " in rule " ++ rname
          | any (\x->case x of (PAtom "_") -> True; _ -> False) args =
            error $ "Wildcard in positive set condition for the set " ++ set ++ " in rule " ++ rname
          | otherwise = checkSetCondTypes (PCond allQ var (PComp set args))
        checkNegSetCond (PCond allQ var (PComp set args)) = do
          -- TODO: check more about negative set conditions:
          -- * all infinite-range type parameters need to be universally quantified
          checkAll checkParam (zip (map atomName args) argTypes);
          checkSetCondTypes (PCond allQ var (PComp set args))
          where
              argTypes = setTypes set
              checkParam ("_",_) = return ()
              checkParam (x, ty) =
                if x `elem` allQ || isFiniteType localEnv ty then return ()
                else error $ "Variable " ++ x ++ " used as argument to set " ++ set ++ " in rule " ++ rname ++ " is not all-quantified and ranges over an infinite type"
        checkSetCondTypes (PCond allQ var (PComp set args)) =
          case find (not . uncurry argSubtypeOf) (zip args argTypes) of
            Just (PAtom t1, t2) -> error $ "Variable " ++ t1 ++ " used in set " ++ set ++ " in rule " ++ rname ++ " is not a subtype of the declared type " ++ t2
            Nothing -> return ()
          where argSubtypeOf x ty = case x of
                  PAtom "_" -> True
                  PAtom y -> isSubtypeOf localEnv y ty
                  _ -> error "Expecting variable here"
                argTypes = setTypes set
        -- TODO: check that the typing environment has CamelCase names only!
        -- TODO: check that the rules are consistent
        setTypes set = case find (\(PComp set' _) -> set == set') (setdecs p) of
          Just (PComp _ types) -> map atomName types
          Nothing -> error $ "Set declaration for " ++ set ++ " not found"


--- Copied from FPASLan.hs
m ! k = fromMaybe (error $ k++" undefined") $ Map.lookup k m

getvals :: Symbtab -> PIdent -> [Substitution]
getvals tab var =
  case tab ! var of
    VarEnu vals -> [Map.singleton var (PAtom v) | v<-vals]

quantelimns :: Symbtab -> [PCond] -> [PCond]
quantelimns tab = concatMap $ quantelimn tab

quantelimn :: Symbtab -> PCond -> [PCond]
quantelimn tab (PCond qs t s) =
  nub $
  foldr (\ var conds ->
           concat [subcs sigma conds | sigma <- getvals tab var])
        [PCond [] t s] qs

typeId :: PType -> PIdent
typeId (VarOf x) = x

quantelim :: Symbtab -> PRule -> [PRule]
quantelim tab (rule@(PRule name lamvar lf ps ns f rf rs)) =
  let nsqf = quantelimns tab ns in
  foldr ((\ var rules ->
           concat [subrs sigma rules | sigma <- getvals tab var ]) . typeId . snd)
         [PRule { ruleName = name, ruleEnv = [],
                   lf = lf, lp = ps, ln = nsqf,
                   fresh = f, rf = rf, rp = rs }] lamvar
-- end of copy


zero = PComp "0" []

compile :: PASLan -> PASLan
compile p = p
   { typedec = [], rules = newRules, privfundec = newPrivfundec,
     -- change by zeb: all is<Type> facts implied by user-defined types are added to the list of facts
     facdec = ("timplies",2):userTypeFactDecs++facdec p,
     -- another change by zeb: all enum-types are arity zero functions
     pubfundec = ("zero",0):concatMap getEnum (typedec p)++pubfundec p
   }
  where
    getEnum (_,EnumType names) = map (\ x->(x,0)) names
    getEnum _ = []
    newRules = nub $ userTypeRules ++ concatMap compileRule (rules p)
    newPrivfundec = [(name, length args) | (PComp name args) <- setdecs p] ++
      privfundec p ++ [(typeFuncName id, 1) | (id, CountType) <- typedec p]
    typeFactName (i:id) = "is" ++ toUpper i : id -- TODO: ensure there is no conflict with userdef idents
    typeFuncName (i:id) = toLower i : id         -- TODO: ensure there is no conflict with userdef idents
    userTypeFactDecs = map (\ (id, ty) -> (typeFactName id, 1)) (typedec p)
    userTypeRules = concatMap
      (\ (id, ty) -> case ty of
          EnumType names ->
            [PRule { ruleName = "userType", ruleEnv = [],
                     lf = [], lp = [], ln = [], fresh = [], rp = [],
                     rf = [PFact (typeFactName id) [PAtom name] | name <- names]}]
          CountType ->
            [PRule { ruleName = "userType", ruleEnv = [],
                     lf = [], lp = [], ln = [], fresh = [], rp = [],
                     rf = [PFact (typeFactName id) [PComp (typeFuncName id) [PAtom "X"]]]}]
          Union types ->
            [PRule { ruleName = "userType", ruleEnv = [],
                     lp = [], ln = [], fresh = [], rp = [],
                     lf = [PFact (typeFactName t) [PAtom "X"]],
                     rf = [PFact (typeFactName id) [PAtom "X"]]} | t <- types]
          _ -> error "AIFOm.compile: no global variables are allowed"
      ) (typedec p)
    compileRule r =
      r { ruleEnv = [],
          lf = typesFacts ++ subfs lambda (lf r),
          rf = subfs rho (rf r) ++ timpliesFacts,
          lp = [], ln = [], fresh = [], rp = [] } : timpliesRules
      where
        left s x =
          case find (\(PCond _ x' (PComp s' _)) -> s == s' && x == x') (lp r) of
            Just (PCond _ _ (PComp _ args)) -> PComp s args
            Nothing ->
              case find (\(PCond _ x' (PComp s' _)) -> s == s' && x == x') (ln r) of
                Just (PCond _ _ (PComp _ args)) -> zero
                Nothing -> PAtom ("E__" ++ s ++ "__" ++ x)
        right s x =
          case find (\(PCond _ x' (PComp s' _)) -> s == s' && x == x') (rp r) of
            Just (PCond _ _ (PComp _ args)) -> PComp s args
            Nothing ->
              case (x `elem` fresh r, left s x) of
                (False, PAtom e_i_x) -> PAtom e_i_x
                _ -> zero
        leftVal x = PComp "val" [left s x | PComp s _ <- setdecs p]
        rightVal x = PComp "val" [right s x | PComp s _ <- setdecs p]
        lambda = Map.fromList [(x, leftVal x) | (x, Value) <- ruleEnv r]
        rho = Map.fromList
          ([(x, rightVal x) | (x, Value) <- ruleEnv r] ++
           [(x, rightVal x) | x <- fresh r])
        timpliesFacts =
          [PFact "timplies" [leftVal x, rightVal x] |
           (x, ty) <- ruleEnv r, x `notElem` fresh r, ty == Value, leftVal x /= rightVal x]
        typesFacts =
          [PFact (typeFactName ty) [PAtom x] | (x, VarOf ty) <- ruleEnv r]
        timpliesRules =
          [PRule { ruleName = "timplies", ruleEnv = [],
                   lf = typesFacts ++
                        [subf sigma_1 fact,
                         PFact "timplies" [PAtom "Val_1", PAtom "Val_2"]],
                   rf = [subf sigma_2 fact],
                   lp = [], ln = [], fresh = [], rp = [] }
          | fact <- rf r, x <- fvf fact,
            getEnv (ruleEnv r) x == Value,
            let sigma_1 = Map.fromList [(x, PAtom "Val_1")],
            let sigma_2 = Map.fromList [(x, PAtom "Val_2")]]
