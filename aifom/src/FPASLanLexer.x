{

{-

AIF Omega version 2016

Developed under BSD license as part of OFMC

(C) Copyright Alessandro Bruni 2015,2016
(C) Copyright DTU 2010,2016
(C) Copyright Paolo Modesti 2010
(C) Copyright IBM Corp. 2010

All Rights Reserved.




List of all keywords and names:   --- string   ^\([^ ]*\)[^T]*\([a-zA-Z]*\).*

Problem:    Tproblem
Types:      Ttypes
Sets:       Tsets
Functions:  Tfunctions
public      Tpublic
private     Tprivate
Facts:      TFacts
Rules:      TRules
value       Tvalue
untyped     Tuntyped
notin       Tnotin
in          Tin
forall      Tforall
exists      Fexists
Definitions: TDefinitions

List of all symbols and names:

;    Tsemicolon
:    Tcolon
,    Tcomma
{    Topenbrace
}    Tclosebrace
[    Topensquare
]    Tclosesquare
(    Topenpar
)    Tclosepar
/    Tslash
\    Tlambda
++   Tunion
.    Tdot
_    Twild
=>   Tarrow
=[   Topenarrow
]=>  Tclosarrow

Further tokens:

ident Tident
int   Tint

-}

module FPASLanLexer (Token(..), PIdent, AlexPosn(..), alexScanTokens
                  , token_posn
                  ) where
import Types
}

%wrapper "posn"

$digit = 0-9   -- digits
$alpha = [a-zA-Z]  -- alphabetic characters
$identChar = [a-zA-Z0-9_]
tokens :-

  $white+;
  "%".*;
  "#" ($printable # '#' | $white)* "#";

"Problem:" {(\ p s -> Tproblem p)}
"Types:" {(\ p s -> Ttypes p)}
"Sets:" {(\ p s -> Tsets p)}
"Functions:" {(\ p s -> Tfunctions p)}
"public" {(\ p s -> Tpublic p)}
"private" {(\ p s -> Tprivate p)}
"Facts:" {(\ p s -> TFacts p)}
"Rules:" {(\ p s -> TRules p)}
"Definitions:" {(\ p s -> TDefinitions p)}
"value" {(\ p s -> Tvalue p)}
"untyped" {(\ p s -> Tuntyped p)}
"notin" {(\ p s -> Tnotin p)}
"in" {(\ p s -> Tin p)}
"forall" { (\ p s -> Tforall p)}
"exists" { (\ p s -> Texists p)}
";" {(\ p s -> Tsemicolon p)}
":" {(\ p s -> Tcolon p)}
"," {(\ p s -> Tcomma p)}
"{" {(\ p s -> Topenbrace p)}
"}" {(\ p s -> Tclosebrace p)}
"[" {(\ p s -> Topensquare p)}
"]" {(\ p s -> Tclosesquare p)}
"(" {(\ p s -> Topenpar p)}
")" {(\ p s -> Tclosepar p)}
"/" {(\ p s -> Tslash p)}
"\" {(\ p s -> Tlambda p)}
"++" {(\ p s -> Tunion p)}
"." {(\ p s -> Tdot p)}
"_" {(\ p s -> Twild p)}
"=>" {(\ p s -> Tarrow p)}
"=[" {(\ p s -> Topenarrow p)}
"]=>" {(\ p s -> Tclosarrow p)}
"=" {(\ p s -> Tequal p)}

$alpha$identChar* { (\ p s -> Tident p s)}
$digit+ { (\p s -> Tint p (read s))}

{

data Token=
       Tident AlexPosn PIdent
     | Tint AlexPosn Int
     | Tproblem AlexPosn
     | Ttypes AlexPosn
     | Tsets AlexPosn
     | Tfunctions AlexPosn
     | Tpublic AlexPosn
     | Tprivate AlexPosn
     | TFacts AlexPosn
     | TRules AlexPosn
     | TDefinitions AlexPosn
     | Tvalue AlexPosn
     | Tuntyped AlexPosn
     | Tnotin AlexPosn
     | Tin AlexPosn
     | Tforall AlexPosn
     | Texists AlexPosn
     | Tsemicolon AlexPosn
     | Tcolon AlexPosn
     | Tcomma AlexPosn
     | Topenbrace AlexPosn
     | Tclosebrace AlexPosn
     | Topensquare AlexPosn
     | Tclosesquare AlexPosn
     | Topenpar AlexPosn
     | Tclosepar AlexPosn
     | Tslash AlexPosn
     | Tlambda AlexPosn
     | Tunion AlexPosn
     | Tdot AlexPosn
     | Twild AlexPosn
     | Tarrow AlexPosn
     | Topenarrow AlexPosn
     | Tclosarrow AlexPosn
     | Tequal AlexPosn
     deriving (Eq,Show)

token_posn (Tident p s) = p
token_posn (Tint p i) = p
token_posn (Tproblem p) = p
token_posn (Ttypes p) = p
token_posn (Tsets p) = p
token_posn (Tfunctions p) = p
token_posn (Tpublic p) = p
token_posn (Tprivate p) = p
token_posn (TFacts p) = p
token_posn (TRules p) = p
token_posn (TDefinitions p) = p
token_posn (Tvalue p) = p
token_posn (Tuntyped p) = p
token_posn (Tnotin p) = p
token_posn (Tin p) = p
token_posn (Tforall p) =p
token_posn (Texists p) =p
token_posn (Tsemicolon p) = p
token_posn (Tcolon p) = p
token_posn (Tcomma p) = p
token_posn (Topenbrace p) = p
token_posn (Tclosebrace p) = p
token_posn (Topensquare p) = p
token_posn (Tclosesquare p) = p
token_posn (Topenpar p) = p
token_posn (Tclosepar p) = p
token_posn (Tslash p) = p
token_posn (Tlambda p) = p
token_posn (Tunion p) = p
token_posn (Tdot p) = p
token_posn (Twild p) = p
token_posn (Tarrow p) = p
token_posn (Topenarrow p) = p
token_posn (Tclosarrow p) = p
token_posn (Tequal p) = p

}
