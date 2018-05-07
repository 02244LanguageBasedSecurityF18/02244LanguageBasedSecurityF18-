{

{-

Open Source Fixedpoint Model-Checker version 2010

(C) Copyright IBM Corp. 2010

All Rights Reserved.

Modifications by Paolo Modesti 2010

-}

module FPASLanParser where
import Data.Char
import Data.List
import FPASLanLexer
import Types
}


%name parser
%tokentype {Token}

%token	    ident		{Tident _ $$}
	    int			{Tint _ $$}

	    'Problem:' {Tproblem _}
	    'Types:' {Ttypes _}
	    'Sets:' {Tsets _}
	    'Functions:' {Tfunctions _}
	    'public' {Tpublic _}
	    'private' {Tprivate _}
	    'Facts:' {TFacts _}
	    'Rules:' {TRules _}
	    'Definitions:' {TDefinitions _}
	    'value' {Tvalue _}
	    'untyped' {Tuntyped _}
	    'notin' {Tnotin _}
	    'in' {Tin _}
	    'forall' {Tforall _}
	    'exists' {Texists _}
	    ';' {Tsemicolon _}
	    ':' {Tcolon _}
	    ',' {Tcomma _}
	    '{' {Topenbrace _}
	    '}' {Tclosebrace _}
	    '[' {Topensquare _}
	    ']' {Tclosesquare _}
	    '(' {Topenpar _}
	    ')' {Tclosepar _}
	    '/' {Tslash _}
	    '\\' {Tlambda _}
      '++' {Tunion _}
	    '.' {Tdot _}
			'_' {Twild _}
	    '=>' {Tarrow _}
	    '=[' {Topenarrow _}
	    ']=>' {Tclosarrow _}
      '=' {Tequal _}


%%


FPASLan :: {PASLan}
: 'Problem:'   ident ';'
  maybe_types
	maybe_sets
	functions
  'Facts:'     symdecs ';'
-- <paolo>
    defdec
-- </paolo>
  'Rules:'     rules
{ let (pubfn, privfn) = $6 in (PASLan $2 $4 $5 pubfn privfn $8 $10 $12) }

maybe_types
: 'Types:'     typedecs { $2 }
| {[]}

maybe_sets :: {[PTerm]}
: 'Sets:' terms ';' { map atomToComp $2 }
| {[]}

functions :: {([(PIdent,Int)], [(PIdent,Int)])}
: 'Functions:' 'public' symdecs ';'
						   'private' symdecs ';'
	{($3, $6)}
| 'Functions:' symdecs ';'
	{($2, [])}

-- <paolo>
defdec :: {PDefinitions}
: {[]}
|'Definitions:' deflist {$2}

deflist :: {PDefinitions}
: ident ':' term {[($1,$3,[])]}
| ident '[' idents ']' ':' term {[($1,$6,$3)]}
| ident ':' term ';' deflist {(($1,$3,[]):$5)}
| ident '[' idents ']' ':' term ';' deflist {(($1,$6,$3):$8)}

-- </paolo>

typedecs :: {[Typedec]}
: idents ':' var_type ';' typedecs {([(x,$3) | x <- $1] ++ $5)}
| ident '=' user_type ';' typedecs {(($1,$3):$5)}
| {[]}

idents_set :: {[PIdent]}
: ident {[$1]}
| ident ',' idents_set { $1:$3 }

idents :: {[PIdent]}
: ident {[$1]}
| ident ',' idents { ($1:$3) }

type_env :: {[Vardec]}
: ident ':' var_type_expl { [($1, $3)] }
| ident ':' var_type_expl ',' type_env { ($1, $3) : $5 }

-- Note: when using the old lambda notation, empty type references are inserted.
-- It is therefore necessary to substitute these with the proper global types
-- during normalization.
global_type_env :: {[Vardec]}
: ident { [($1, VarOf "")] }
| ident ',' global_type_env { ($1, VarOf "") : $3 }

unions :: {[PIdent]}
: ident {[$1]}
| ident '++' unions {($1:$3)}

var_type :: {PType}
: '{' idents_set '}' {Enum $2}
| var_type_expl {$1}

var_type_expl :: {PType}
: ident {VarOf $1}
| 'value' {Value}
| 'untyped' {Untyped}

user_type :: {PType}
: '{' idents_set '}' {EnumType $2}
| '{' '.' '.' '.' '}' {CountType}
| unions {Union $1}

terms :: {[PTerm]}
: term {[$1]}
| term ',' terms {($1:$3)}

term :: {PTerm}
: ident  {PAtom $1}
| '_' {PAtom "_"}
| ident '(' terms ')' {PComp $1 $3}
| ident '[' terms ']' {PPar $1 $3}
--<paolo>
| '[' ident ']' {PParId $2}
--</paolo>
symdecs :: {[(PIdent,Int)]}
: ident '/' int {[($1,$3)]}
| ident '/' int ',' symdecs {($1,$3):$5}

rules :: {[PRule]}
: rule rules {$1:$2}
| {[]}

rule :: {PRule}
: lambda lhss arrow rhss ';'
{  (\ (nm,env) (lf,lp,ln) (rf,rp,_) ->
      PRule { ruleName = nm, ruleEnv = env,
              lf = lf, lp = lp, ln = ln,
              fresh = $3, rf = rf, rp = rp }) $1 $2 $4
}

lambda :: {(PIdent, [Vardec])}
: '\\' global_type_env '.' {("lambda", $2)}
| ident '(' type_env ')' {($1, $3)}
| {("lambda", [])}

lhss :: {([PFact],[PCond],[PCond])}
: lhs '.' lhss {combside $1 $3}
| {([],[],[])}
| lhs {$1}

rhss :: {([PFact],[PCond],[PCond])}
: rhs '.' rhss {combside $1 $3}
| {([],[],[])}
| rhs {$1}

lhs:: {([PFact],[PCond],[PCond])}
: rhs {$1}
| ident 'notin' term {[],[],[(PCond [] $1 (atomToComp $3))]}
| 'forall' idents '.' ident 'notin' term {[],[],[(PCond $2 $4 (atomToComp $6))]}

rhs:: {([PFact],[PCond],[PCond])}
: pfact {([$1],[],[])}
| ident 'in' term {[],[(PCond [] $1 (atomToComp $3))],[]}

pfact:: {PFact}
: ident '(' terms ')' {(PFact $1 $3)}
| ident  {(PFact $1 [])}

arrow :: {[PIdent]}
: '=>' {[]}
| '=[' idents ']=>' {$2}


{
happyError :: [Token] -> a
happyError tks = error ("Parse error at " ++ lcn ++ "\n" )
	where
	lcn = 	case tks of
		  [] -> "end of file"
		  tk:_ -> "line " ++ show l ++ ", column " ++ show c ++ " - Token: " ++ show tk
			where
			AlexPn _ l c = token_posn tk
}
