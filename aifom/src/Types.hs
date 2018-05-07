{-

AIF Omega version 2016

Developed under BSD license as part of OFMC

(C) Copyright Alessandro Bruni 2015,2016
(C) Copyright DTU 2010,2016
(C) Copyright Paolo Modesti 2010
(C) Copyright IBM Corp. 2010

All Rights Reserved.

-}

module Types where

import Data.List
import Data.Char
import qualified Data.Map as Map

data Output = Proverif | Isabelle | SPASS deriving (Eq,Show)

type PIdent = String
type Funcdec = (PIdent,Int)
type Factdec = (PIdent,Int)

data PASLan = PASLan
              { name :: PIdent,
                typedec :: [Typedec],
                setdecs :: [PTerm],
                pubfundec :: [Funcdec],
                privfundec :: [Funcdec],
                facdec :: [Factdec],
                macros :: PDefinitions,
                rules :: [PRule] }
              deriving (Eq,Show)

type Typedec = (PIdent,PType)
data PType =
-- Types for variables
    Enum [PIdent]  -- e.g. X:{ c1,c2,c3 } (Anonymous user types)
  | VarOf PIdent -- e.g. X:T
  | Value -- e.g. X : Value
  | Untyped -- e.g. X : Untyped
-- User-defined types
  | EnumType [PIdent] -- e.g. T={c1,c2,c3}
  | CountType  -- e.g. T={...}
  | Union [PIdent] -- e.g. T=T1+T2+T3
  deriving (Eq,Show)

type Vardec = (PIdent,PType)
data PTerm = PAtom {atomName :: PIdent}
           | PComp {compName :: PIdent, compArgs :: [PTerm]}
-- <paolo>
           | PPar PIdent [PTerm]
           | PParId PIdent
-- </paolo>
           deriving (Eq,Show,Ord)

isConst (PAtom (x:xs)) = x `elem` ['a'..'z']
isConst _ = False

data PFact = PFact {factName :: PIdent, factArgs :: [PTerm]}
           deriving (Eq,Show,Ord)

data PCond = PCond {condForall :: [PIdent], condVar :: PIdent, condSet :: PTerm}
           deriving (Eq,Show,Ord)

data PRule = PRule { ruleName :: String,
                     ruleEnv  :: [Vardec], -- type env (name, type)
                     lf :: [PFact], -- left facts
                     lp :: [PCond], -- left positive set conditions
                     ln :: [PCond], -- left negative set conditions
                     fresh :: [PIdent], -- fresh names
                     rf :: [PFact], -- right facts
                     rp :: [PCond] -- right positive set conditions
                   } deriving (Eq, Show)

-- <paolo>
type PDefinition = (PIdent,PTerm,[PIdent])
type PDefinitions = [PDefinition]
-- </paolo>
combside :: ([PFact],[PCond],[PCond]) -> ([PFact],[PCond],[PCond]) -> ([PFact],[PCond],[PCond])
combside (pf,ps,ns) (pf',ps',ns') = (pf++pf',ps++ps',ns++ns')

type Symbtab = Map.Map String SymbEntry

data SymbEntry = EnuCo
               | VarEnu [String]
               | VarVal
               | ConVal
               | VarUntyped
               | FunSym Bool Int
               | FactSym Int
               | SetSym [String]
               deriving (Eq,Show)


-- Support functions
isEnu (VarEnu _) = True
isEnu _ = False
isVal VarVal = True
isVal ConVal = True
isVal _ = False
isVar = isUpper . head
isSet (SetSym _) = True
isSet _ = False
isFun :: SymbEntry -> Bool
isFun (FunSym _ _) = True
isFun _ = False
isFact (FactSym _) = True
isFact _ = False
isEnuCo :: SymbEntry -> Bool
isEnuCo EnuCo = True
isEnuCo _ = False
isPub (FunSym True _) = True
isPub _ = False
arity :: SymbEntry -> Int
arity (FunSym _ n) = n
arity (FactSym n)  = n
arity EnuCo = 0
arity (VarEnu _) = 0
arity VarVal = 0
arity ConVal =0
arity VarUntyped =0
arity (SetSym l) = length l

seti :: [a] -> Int -> a -> [a]
seti list i v = let (l1,l2)=splitAt (i-1) list
                in l1++[v]++tail l2

-- Free variables
fvfs = nub . concatMap fvf
fvf (PFact fsym ts) = fvts ts

fvts = nub . concatMap fvt
fvt (PAtom p) = [p | isVar p]
fvt (PComp f ts) = fvts ts
--  defintions
fvt (PParId p) = error $ "error in definitions translation - Illegal PParId "++p
fvt (PPar f ts) = error $ "error in definitions translation - Illegal PPar "++f
-- definitions
fvss = nub . concatMap fvs
fvs (PCond uni t s) = nub ([t | isVar t]++fvt s) \\ uni
-- rules
fvrs = nub . concatMap fvr
fvr (PRule _ env lf lp ln fr rf rp) =
  nub (fvfs lf ++ fvss lp ++ fvss ln ++ fvfs rf ++ fvss rp) \\ map fst env

type Substitution = Map.Map PIdent PTerm

subrs :: Substitution -> [PRule] -> [PRule]
subr  :: Substitution ->  PRule  ->  PRule
subfs :: Substitution -> [PFact] -> [PFact]
subf  :: Substitution ->  PFact  ->  PFact
subcs :: Substitution -> [PCond] -> [PCond]
subc  :: Substitution ->  PCond  ->  PCond
subts :: Substitution -> [PTerm] -> [PTerm]
subt  :: Substitution ->  PTerm  ->  PTerm

subrs sub = map $ subr sub
subr sub (PRule name env lf ps ns f rf rs) =
          PRule {
             ruleName = name,
             ruleEnv  = filter (\(x,y) -> x `notElem` Map.keys sub) env,
             lf = subfs sub lf,
             lp = subcs sub ps,
             ln = subcs sub ns,
             fresh = f,
             rf = subfs sub rf,
             rp = subcs sub rs }
subfs sub = map (subf sub)
subf  sub (PFact f ts) = PFact f (subts sub ts)
subcs sub = map (subc sub)
subc  sub (PCond allq t set) = PCond allq t (subt sub set)
subts sub = map (subt sub)
subt  sub (PAtom a) = Map.findWithDefault (PAtom a) a sub
subt  sub (PComp f ts) = PComp f (subts sub ts)

subt  sub (PParId a) = error ("error in definitions translation - PParId " ++ a)
subt  sub (PPar f ts) = error ("error in definitions translation - PPar " ++ f)

-- Insert a value in the sybol table only if not already present
insertOnce :: String -> SymbEntry -> Symbtab -> Symbtab
insertOnce k v m = if Map.member k m then error $ "Multiple Decs of " ++ k
                   else Map.insert k v m

insertMultiple :: [(String, SymbEntry)] -> Symbtab -> Symbtab
insertMultiple kv m = foldr (\ (k,v) tab -> insertOnce k v tab) m kv

atomToComp x = case x of (PComp _ _) -> x; (PAtom x') -> PComp x' []
