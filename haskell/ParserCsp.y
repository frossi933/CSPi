{
module ParserCsp where

import Common
import Csp
import Data.Char

}

%name cspparser
%monad { IO }
%tokentype {Token}
%error {parseError}
%token
    SKIP        {TSkip}
    STOP        {TStop}
    PNAME       {TPName $$}
    EONAME      {TEOName $$}
    EINAME      {TEIName $$}
    EXP         {TExp $$}
    VAR         {TVar $$}
    PRED        {TPred $$}
    ACT         {TAct $$}
    EVENT       {TEvent}
    FROM        {TFrom}
    OCCURS      {TOccurs}
    WHEN        {TWhen}
    DOES        {TDoes}
    '='         {TDef}
    '?'         {TCIn}
    '!'         {TCOut}
    '('         {TOpen}
    ')'         {TClose}
    ','         {TComa}
    '->'        {TPrefix}
    '||'        {TPar}
    '[]'        {TExtSel}
    '/|'        {TIntSel}
    ';'         {TSeq}
    '|>'        {TInter}
    

%left '||' '[]' '/|' '|>' ';' 
%right '->'
%%

Stmt : Defs                                     { ($1, []) }
     | Defs Claus                               { ($1, $2) }
     
Claus : Clau                                    { [$1] }
      | Clau Claus                              { $1:$2 }
      
Clau : EVENT EINAME FROM PNAME OCCURS WHEN PRED { CPred $2 $4 $7 }
     | EVENT EINAME FROM PNAME DOES ACT         { CAct $2 $4 $6 }

Defs : Def                                      { [$1] }
     | Def Defs                                 { $1:$2 }
     
Def : PNAME '=' Proc                            { Def $1 [] $3 }
    | PNAME '(' Args ')' '=' Proc               { Def $1 $3 $6 }
    
Args : EXP                                      { [$1] }
     | EXP ',' Args                             { $1:$3 }

Proc    : STOP                                  { Stop }
        | SKIP                                  { Skip }
        | '(' Proc ')'                          { $2 }
        | Event '->' Proc                       { Prefix $1 $3 }
        | RefProc                               { $1 }
        | Proc '||' Proc                        { Parallel $1 $3 }
        | Proc '[]' Proc                        { ExtSel [$1,$3] }
        | Proc '/|' Proc                        { IntSel $1 $3 }
        | Proc ';' Proc                         { Seq $1 $3 }
        | Proc '|>' Proc                        { Inter $1 $3 }

       
Event   : EventIn                               { In $1 "" }
        | EventOut                              { Out $1 "" }
        | EventIn '?' VAR                       { C (ComIn $1 $3) }
        | EventIn '!' EXP                       { C (ComOut $1 $3) }
        
EventIn : EINAME                                { $1 }

EventOut: EONAME                                { $1 }

RefProc : PNAME                                 { Ref $1 [] }
        | PNAME '(' Args ')'                    { Ref $1 $3 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token =  TSkip
            | TStop
            | TOpen
            | TClose
            | TComa
            | TPName String
            | TEOName String
            | TEIName String
            | TExp String
            | TVar String
            | TPred String
            | TAct String
            | TEvent
            | TFrom
            | TOccurs
            | TWhen
            | TDoes
            | TDef
            | TCIn
            | TCOut
            | TPrefix
            | TPar
            | TExtSel
            | TIntSel
            | TSeq
            | TInter


lexer :: String -> [Token]
lexer []             = []
lexer ('=':cs)       = TDef : (lexer cs)
lexer ('{':('-':cs)) = lexCom cs
lexer ('(':cs)       = TOpen : (lexer cs)
lexer (')':cs)       = TClose : (lexer cs)
--lexer (',':cs)       = T
lexer ('-':('>':cs)) = TPrefix : (lexer cs)
lexer ('|':('|':cs)) = TPar : (lexer cs)
lexer ('[':(']':cs)) = TExtSel : (lexer cs)
lexer ('/':('|':cs)) = TIntSel : (lexer cs)
lexer (';':cs)       = TSeq : (lexer cs)
lexer ('|':('>':cs)) = TInter : (lexer cs)
lexer (c:cs)
        | isSpace c = lexer cs
        | c == '_' = case fstWord cs of
                        ("",cont) -> [] --Failed $ "Error de nombre de evento"
                        ((n:ns),cont) -> (TEOName (n:ns)) : lexer cont                          -- lo guardo sin el "_", revisar...
        | isAlpha c = lexWord (c:cs)
lexer unknown =  []  --Failed $ "No se puede reconocer "++(show $ take 10 unknown)++ "..."

lexWord [] = []
lexWord (c:cs)  | isUpper c = case fstWord (c:cs) of
                                ("STOP", cont)  -> TStop : lexer cont
                                ("SKIP", cont)  -> TSkip : lexer cont
                                (p, ('(':cont)) -> let (e, contt) = paren cont in [TPName p, TOpen, TExp e, TClose]++(lexer (tail contt)) 
                                (p, cont)       -> (TPName p):lexer cont
                | isLower c = case fstWord (c:cs) of
                                ("event", cont)  -> TEvent : lexer cont
                                ("from", cont)   -> TFrom : lexer cont
                                ("occurs", cont) -> TOccurs : lexer cont
                                ("when", cont)   -> let (p, contt) = fstWord (dropWhile isSpace cont) in (TWhen : ((TPred p) : lexer contt))
                                ("does", cont)   -> let (a, contt) = fstWord (dropWhile isSpace cont) in (TDoes : ((TAct a) : lexer contt))
                                (e, ('?' : cont))  -> let (v, contt) = fstWord cont in [TEIName e, TCIn, TVar v]++ (lexer contt)
                                (e, ('!' : cont))  -> let (exp, contt) = paren cont in [TEIName e, TCOut, TExp exp]++(lexer (tail contt))
                                (e, cont)        -> (TEIName e) : lexer cont
                
fstWord = span (\c -> isAlpha c || c == '_' || isDigit c)

paren ('(':rest) = span (\c -> c /= ')') rest           --(takeWhile (\c -> c /= ')') rest) ++ (tail $ dropWhile (\c -> c /= ')') rest)
paren x = span (\c -> c/= ' ') x

lexCom ('-':('}':cs)) = lexer cs
lexCom (c:cs) = lexCom cs
}