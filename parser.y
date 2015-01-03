{
module ParserCsp where

import Csp
import Data.Char

}

%name cspparser
%tokentype {Token}
%error {errorParser}
%token
    SKIP        {TSkip}
    STOP        {TStop}
    PNAME       {TPName $$}
    EONAME      {TEOName $$}
    EINAME      {TEIName $$}
    '->'        {TPrefix}
    '|'         {TChoice}
    '||'        {TPar}
    '[]'        {TExtSel}

%left '|'
%left '||' '[]'    
%right '->'
%%

Proc    : STOP                                          { Stop }
        | SKIP                                          { Skip }
        | Event '->' Proc                               { Prefix $1 $3 }
        | RefProc                                       { Ref $1 }
        | Event '->' Proc '|' Event '->' Proc           { Choice $1 $3 $5 $7 }
        | Proc '||' Proc                                { Parallel [$1,$3] }
        | Proc '[]' Proc                                { ExtSel [$1,$3] }
        
Event   : EventIn                               { In $1 (\x-> return ()) }
        | EventOut                              { Out $1 True }

EventIn : EINAME                                { $1 }

EventOut: EONAME                                { $1 }

RefProc : PNAME                                 { $1 }

{
parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token =  TSkip
            | Tstop
            | TPName String
            | TEOName String
            | TEIName String
            | TPrefix
            | TChoice
            | TPar
            | TExtSel


lexer :: String -> [Token]
lexer [] = []
lexer ('->':cs) = TPrefix : (lexer cs)
lexer ('|':cs) = TChoice : (lexer cs)
lexer ('||':cs) = TPar : (lexer cs)
lexer ('[]':cs) = TExtSel : (lexer cs)
lexer (c:cs)
        | isSpace c = lexer cs
        | isAlpha c = lexName (c:cs)
lexer unknow =  Failed $ "No se puede reconocer "++(show $ take 10 unknown)++ "..."

lexName [] = []
lexName ('_':(c:cs)) = case fstWord c:cs of
                        ("",cont) -> Failed $ "Error de nombre de evento"
                        ((n:ns),cont) -> (TOEName ("_"++(n:ns))) : lexer cont
lexName (c:cs)  | isUpper c = let (p,cont) = fstWord c:cs in (TPName p):lexer cont
                | isLower c = let (p,cont) = fstWord c:cs in (TEIName p):lexer cont
                
fstWord = span isAlpha
            
}