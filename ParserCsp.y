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
    '='         {TDef}
    '->'        {TPrefix}
    '||'        {TPar}
    '[]'        {TExtSel}
    

%left '||' '[]'    
%right '->'
%%

Defs : Def                                      { [$1] }
     | Def Defs                                 { $1:$2 }
     
Def : PNAME '=' Proc                            { Def $1 $3 }

Proc    : STOP                                          { Stop }
        | SKIP                                          { Skip }
        | Event '->' Proc                               { Prefix $1 $3 }
        | RefProc                                       { Ref $1 Skip }
        | Proc '||' Proc                                { Parallel $1 $3 }
        | Proc '[]' Proc                                { ExtSel [$1,$3] }

       
Event   : EventIn                               { In $1 () }
        | EventOut                              { Out $1 True }

EventIn : EINAME                                { $1 }

EventOut: EONAME                                { $1 }

RefProc : PNAME                                 { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token =  TSkip
            | TStop
            | TPName String
            | TEOName String
            | TEIName String
            | TDef
            | TPrefix
            | TPar
            | TExtSel


lexer :: String -> [Token]
lexer [] = []
lexer ('=':cs) = TDef : (lexer cs)
lexer ('-':('>':cs)) = TPrefix : (lexer cs)
lexer ('|':('|':cs)) = TPar : (lexer cs)
lexer ('[':(']':cs)) = TExtSel : (lexer cs)
lexer (c:cs)
        | isSpace c = lexer cs
        | c == '_' = case fstWord cs of
                        ("",cont) -> [] --Failed $ "Error de nombre de evento"
                        ((n:ns),cont) -> (TEOName ("_"++(n:ns))) : lexer cont
        | isAlpha c = lexName (c:cs)
lexer unknown =  []  --Failed $ "No se puede reconocer "++(show $ take 10 unknown)++ "..."

lexName [] = []
lexName (c:cs)  | isUpper c = case fstWord (c:cs) of
                                ("STOP", cont) -> TStop : lexer cont
                                ("SKIP", cont) -> TSkip : lexer cont
                                (p, cont)      -> (TPName p):lexer cont
                | isLower c = let (p,cont) = fstWord (c:cs) in (TEIName p):lexer cont
                
fstWord = span (\c -> isAlpha c || c == '_' || isDigit c)
            
}