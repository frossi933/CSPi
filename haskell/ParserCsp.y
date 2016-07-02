{
module ParserCsp where

import Common
import Csp
import Data.Char
import qualified Data.Set as Set
import Control.Concurrent
import System.IO.Unsafe

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
    ','         {TComma}
    '->'        {TPrefix}
    '|{'        {TParOpen}
    '}|'        {TParClose}
    '[]'        {TExtSel}
    '/|'        {TIntSel}
    ';'         {TSeq}
    '|>'        {TInter}
    

%left '[]' '/|' '|>' ';' 
%left '|{' '}|'
%right '->'
%%

Stmt : Defs                                     { ($1, []) }
     | Defs Claus                               { ($1, $2) }
     
Claus : Clau                                    { [$1] }
      | Clau Claus                              { $1:$2 }
      
Clau : EVENT EONAME FROM PNAME OCCURS WHEN PRED { CPred $2 $4 $7 }
     | EVENT EONAME FROM PNAME DOES ACT         { CAct $2 $4 $6 }

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
        | Proc '|{' EvSet '}|' Proc             { Parallel $3 $1 $5 }
        | Proc '[]' Proc                        { ExtSel $1 $3 }
        | Proc '/|' Proc                        { IntSel $1 $3 }
        | Proc ';' Proc                         { Seq $1 $3 }
        | Proc '|>' Proc                        { Inter $1 $3 }

       
Event   : EventIn                               { In $1 (unsafePerformIO (do { v <- newEmptyMVar :: IO (MVar Bool) ; putMVar v True ; return v })) }
        | EventOut                              { Out $1 "" }
        | EventIn '?' VAR                       { C (ComIn $1 $3) }
        | EventIn '!' EXP                       { C (ComOut $1 $3) }

EvSet   :                                       { Set.empty }
        | Event                                 { Set.singleton $1 }
        | Event ',' EvSet                       { Set.insert $1 $3 }
        
EventIn : EINAME                                { $1 }

EventOut: EONAME                                { $1 }

RefProc : PNAME                                 { Ref $1 [] }
        | PNAME '(' Args ')'                    { Ref $1 $3 }

{
parseError :: [Token] -> a
parseError s = error ("Parse error"++(show s))

data Token =  TSkip
            | TStop
            | TOpen
            | TClose
            | TComma
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
            | TParOpen
            | TParClose
            | TExtSel
            | TIntSel
            | TSeq
            | TInter deriving(Show)


lexer :: String -> [Token]
lexer []             = []
lexer ('=':cs)       = TDef : (lexer cs)
lexer ('{':('-':cs)) = lexCom cs
lexer ('(':cs)       = TOpen : (lexer cs)
lexer (')':cs)       = TClose : (lexer cs)
lexer ('-':('>':cs)) = TPrefix : (lexer cs)
lexer ('|':('>':cs)) = TInter : (lexer cs)
lexer ('|':('{':cs)) = TParOpen : (lexPar cs)
lexer ('[':(']':cs)) = TExtSel : (lexer cs)
lexer ('/':('|':cs)) = TIntSel : (lexer cs)
lexer (';':cs)       = TSeq : (lexer cs)
lexer (c:cs)
        | isSpace c = lexer cs
        | c == '_' = case fstWord cs of
                        ("",cont) -> [] --Failed $ "Error de nombre de evento"
                        ((n:ns),cont) -> (TEIName (n:ns)) : lexer cont                          -- lo guardo sin el "_", revisar...
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
                                (e, cont)        -> (TEOName e) : lexer cont
                
fstWord = span (\c -> isAlpha c || c == '_' || isDigit c)

paren ('(':rest) = span (\c -> c /= ')') rest           --(takeWhile (\c -> c /= ')') rest) ++ (tail $ dropWhile (\c -> c /= ')') rest)
paren x = span (\c -> c/= ' ') x

rmvSpc s = takeWhile (\c -> c /= ' ') (dropWhile (\c -> c == ' ') s)

lexCom ('-':('}':cs)) = lexer cs
lexCom (c:cs) = lexCom cs

lexPar cs       = case span (\c -> c /= ',' && c /= '}') cs of
                      ([],cs') -> (TParClose : (lexer cs')) -- armar un conjunto vacio
                      (w,('}':('|':cs'))) -> ((TEIName (rmvSpc w)) : (TParClose : (lexer cs')))
                      (w,(',':cs')) -> ((TEIName (rmvSpc w)) : (TComma : (lexPar cs')))
                      _ -> [] -- error

}
