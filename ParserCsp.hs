{-# OPTIONS_GHC -w #-}
module ParserCsp where

import Common
import Csp
import Data.Char

-- parser produced by Happy Version 1.19.3

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13
	= HappyTerminal (Token)
	| HappyErrorToken Int
	| HappyAbsSyn4 t4
	| HappyAbsSyn5 t5
	| HappyAbsSyn6 t6
	| HappyAbsSyn7 t7
	| HappyAbsSyn8 t8
	| HappyAbsSyn9 t9
	| HappyAbsSyn10 t10
	| HappyAbsSyn11 t11
	| HappyAbsSyn12 t12
	| HappyAbsSyn13 t13

action_0 (16) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_3
action_0 _ = happyFail

action_1 (16) = happyShift action_4
action_1 (7) = happyGoto action_2
action_1 (8) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (16) = happyShift action_4
action_3 (7) = happyGoto action_11
action_3 (8) = happyGoto action_3
action_3 _ = happyReduce_7

action_4 (28) = happyShift action_10
action_4 _ = happyFail

action_5 (39) = happyAccept
action_5 _ = happyFail

action_6 (23) = happyShift action_9
action_6 (5) = happyGoto action_7
action_6 (6) = happyGoto action_8
action_6 _ = happyReduce_1

action_7 _ = happyReduce_2

action_8 (23) = happyShift action_9
action_8 (5) = happyGoto action_24
action_8 (6) = happyGoto action_8
action_8 _ = happyReduce_3

action_9 (18) = happyShift action_23
action_9 _ = happyFail

action_10 (14) = happyShift action_17
action_10 (15) = happyShift action_18
action_10 (16) = happyShift action_19
action_10 (17) = happyShift action_20
action_10 (18) = happyShift action_21
action_10 (31) = happyShift action_22
action_10 (9) = happyGoto action_12
action_10 (10) = happyGoto action_13
action_10 (11) = happyGoto action_14
action_10 (12) = happyGoto action_15
action_10 (13) = happyGoto action_16
action_10 _ = happyFail

action_11 _ = happyReduce_8

action_12 (34) = happyShift action_30
action_12 (35) = happyShift action_31
action_12 (36) = happyShift action_32
action_12 (37) = happyShift action_33
action_12 (38) = happyShift action_34
action_12 _ = happyReduce_9

action_13 (33) = happyShift action_29
action_13 _ = happyFail

action_14 (29) = happyShift action_27
action_14 (30) = happyShift action_28
action_14 _ = happyReduce_20

action_15 _ = happyReduce_21

action_16 _ = happyReduce_14

action_17 _ = happyReduce_11

action_18 _ = happyReduce_10

action_19 _ = happyReduce_26

action_20 _ = happyReduce_25

action_21 _ = happyReduce_24

action_22 (14) = happyShift action_17
action_22 (15) = happyShift action_18
action_22 (16) = happyShift action_19
action_22 (17) = happyShift action_20
action_22 (18) = happyShift action_21
action_22 (31) = happyShift action_22
action_22 (9) = happyGoto action_26
action_22 (10) = happyGoto action_13
action_22 (11) = happyGoto action_14
action_22 (12) = happyGoto action_15
action_22 (13) = happyGoto action_16
action_22 _ = happyFail

action_23 (24) = happyShift action_25
action_23 _ = happyFail

action_24 _ = happyReduce_4

action_25 (16) = happyShift action_44
action_25 _ = happyFail

action_26 (32) = happyShift action_43
action_26 (34) = happyShift action_30
action_26 (35) = happyShift action_31
action_26 (36) = happyShift action_32
action_26 (37) = happyShift action_33
action_26 (38) = happyShift action_34
action_26 _ = happyFail

action_27 (20) = happyShift action_42
action_27 _ = happyFail

action_28 (19) = happyShift action_41
action_28 _ = happyFail

action_29 (14) = happyShift action_17
action_29 (15) = happyShift action_18
action_29 (16) = happyShift action_19
action_29 (17) = happyShift action_20
action_29 (18) = happyShift action_21
action_29 (31) = happyShift action_22
action_29 (9) = happyGoto action_40
action_29 (10) = happyGoto action_13
action_29 (11) = happyGoto action_14
action_29 (12) = happyGoto action_15
action_29 (13) = happyGoto action_16
action_29 _ = happyFail

action_30 (14) = happyShift action_17
action_30 (15) = happyShift action_18
action_30 (16) = happyShift action_19
action_30 (17) = happyShift action_20
action_30 (18) = happyShift action_21
action_30 (31) = happyShift action_22
action_30 (9) = happyGoto action_39
action_30 (10) = happyGoto action_13
action_30 (11) = happyGoto action_14
action_30 (12) = happyGoto action_15
action_30 (13) = happyGoto action_16
action_30 _ = happyFail

action_31 (14) = happyShift action_17
action_31 (15) = happyShift action_18
action_31 (16) = happyShift action_19
action_31 (17) = happyShift action_20
action_31 (18) = happyShift action_21
action_31 (31) = happyShift action_22
action_31 (9) = happyGoto action_38
action_31 (10) = happyGoto action_13
action_31 (11) = happyGoto action_14
action_31 (12) = happyGoto action_15
action_31 (13) = happyGoto action_16
action_31 _ = happyFail

action_32 (14) = happyShift action_17
action_32 (15) = happyShift action_18
action_32 (16) = happyShift action_19
action_32 (17) = happyShift action_20
action_32 (18) = happyShift action_21
action_32 (31) = happyShift action_22
action_32 (9) = happyGoto action_37
action_32 (10) = happyGoto action_13
action_32 (11) = happyGoto action_14
action_32 (12) = happyGoto action_15
action_32 (13) = happyGoto action_16
action_32 _ = happyFail

action_33 (14) = happyShift action_17
action_33 (15) = happyShift action_18
action_33 (16) = happyShift action_19
action_33 (17) = happyShift action_20
action_33 (18) = happyShift action_21
action_33 (31) = happyShift action_22
action_33 (9) = happyGoto action_36
action_33 (10) = happyGoto action_13
action_33 (11) = happyGoto action_14
action_33 (12) = happyGoto action_15
action_33 (13) = happyGoto action_16
action_33 _ = happyFail

action_34 (14) = happyShift action_17
action_34 (15) = happyShift action_18
action_34 (16) = happyShift action_19
action_34 (17) = happyShift action_20
action_34 (18) = happyShift action_21
action_34 (31) = happyShift action_22
action_34 (9) = happyGoto action_35
action_34 (10) = happyGoto action_13
action_34 (11) = happyGoto action_14
action_34 (12) = happyGoto action_15
action_34 (13) = happyGoto action_16
action_34 _ = happyFail

action_35 _ = happyReduce_19

action_36 _ = happyReduce_18

action_37 _ = happyReduce_17

action_38 _ = happyReduce_16

action_39 _ = happyReduce_15

action_40 _ = happyReduce_13

action_41 _ = happyReduce_23

action_42 _ = happyReduce_22

action_43 _ = happyReduce_12

action_44 (25) = happyShift action_45
action_44 (27) = happyShift action_46
action_44 _ = happyFail

action_45 (26) = happyShift action_48
action_45 _ = happyFail

action_46 (22) = happyShift action_47
action_46 _ = happyFail

action_47 _ = happyReduce_6

action_48 (21) = happyShift action_49
action_48 _ = happyFail

action_49 _ = happyReduce_5

happyReduce_1 = happySpecReduce_1  4 happyReduction_1
happyReduction_1 (HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1, [])
	)
happyReduction_1 _  = notHappyAtAll 

happyReduce_2 = happySpecReduce_2  4 happyReduction_2
happyReduction_2 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn7  happy_var_1)
	 =  HappyAbsSyn4
		 ((happy_var_1, happy_var_2)
	)
happyReduction_2 _ _  = notHappyAtAll 

happyReduce_3 = happySpecReduce_1  5 happyReduction_3
happyReduction_3 (HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 ([happy_var_1]
	)
happyReduction_3 _  = notHappyAtAll 

happyReduce_4 = happySpecReduce_2  5 happyReduction_4
happyReduction_4 (HappyAbsSyn5  happy_var_2)
	(HappyAbsSyn6  happy_var_1)
	 =  HappyAbsSyn5
		 (happy_var_1:happy_var_2
	)
happyReduction_4 _ _  = notHappyAtAll 

happyReduce_5 = happyReduce 7 6 happyReduction_5
happyReduction_5 ((HappyTerminal (TPred happy_var_7)) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TPName happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TEIName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (CPred happy_var_2 happy_var_4 happy_var_7
	) `HappyStk` happyRest

happyReduce_6 = happyReduce 6 6 happyReduction_6
happyReduction_6 ((HappyTerminal (TAct happy_var_6)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TPName happy_var_4)) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TEIName happy_var_2)) `HappyStk`
	_ `HappyStk`
	happyRest)
	 = HappyAbsSyn6
		 (CAct happy_var_2 happy_var_4 happy_var_6
	) `HappyStk` happyRest

happyReduce_7 = happySpecReduce_1  7 happyReduction_7
happyReduction_7 (HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 ([happy_var_1]
	)
happyReduction_7 _  = notHappyAtAll 

happyReduce_8 = happySpecReduce_2  7 happyReduction_8
happyReduction_8 (HappyAbsSyn7  happy_var_2)
	(HappyAbsSyn8  happy_var_1)
	 =  HappyAbsSyn7
		 (happy_var_1:happy_var_2
	)
happyReduction_8 _ _  = notHappyAtAll 

happyReduce_9 = happySpecReduce_3  8 happyReduction_9
happyReduction_9 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TPName happy_var_1))
	 =  HappyAbsSyn8
		 (Def happy_var_1 happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happySpecReduce_1  9 happyReduction_10
happyReduction_10 _
	 =  HappyAbsSyn9
		 (Stop
	)

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 _
	 =  HappyAbsSyn9
		 (Skip
	)

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 _
	(HappyAbsSyn9  happy_var_2)
	_
	 =  HappyAbsSyn9
		 (happy_var_2
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_3  9 happyReduction_13
happyReduction_13 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn9
		 (Prefix happy_var_1 happy_var_3
	)
happyReduction_13 _ _ _  = notHappyAtAll 

happyReduce_14 = happySpecReduce_1  9 happyReduction_14
happyReduction_14 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn9
		 (Ref happy_var_1
	)
happyReduction_14 _  = notHappyAtAll 

happyReduce_15 = happySpecReduce_3  9 happyReduction_15
happyReduction_15 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Parallel happy_var_1 happy_var_3
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  9 happyReduction_16
happyReduction_16 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (ExtSel [happy_var_1,happy_var_3]
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_3  9 happyReduction_17
happyReduction_17 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (IntSel happy_var_1 happy_var_3
	)
happyReduction_17 _ _ _  = notHappyAtAll 

happyReduce_18 = happySpecReduce_3  9 happyReduction_18
happyReduction_18 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_18 _ _ _  = notHappyAtAll 

happyReduce_19 = happySpecReduce_3  9 happyReduction_19
happyReduction_19 (HappyAbsSyn9  happy_var_3)
	_
	(HappyAbsSyn9  happy_var_1)
	 =  HappyAbsSyn9
		 (Inter happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_1  10 happyReduction_20
happyReduction_20 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (In happy_var_1 ""
	)
happyReduction_20 _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_1  10 happyReduction_21
happyReduction_21 (HappyAbsSyn12  happy_var_1)
	 =  HappyAbsSyn10
		 (Out happy_var_1 ""
	)
happyReduction_21 _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyTerminal (TCVar happy_var_3))
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (C (ComIn happy_var_1 happy_var_3)
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_3  10 happyReduction_23
happyReduction_23 (HappyTerminal (TCExp happy_var_3))
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (C (ComOut happy_var_1 happy_var_3)
	)
happyReduction_23 _ _ _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyTerminal (TEIName happy_var_1))
	 =  HappyAbsSyn11
		 (happy_var_1
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_1  12 happyReduction_25
happyReduction_25 (HappyTerminal (TEOName happy_var_1))
	 =  HappyAbsSyn12
		 (happy_var_1
	)
happyReduction_25 _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_1  13 happyReduction_26
happyReduction_26 (HappyTerminal (TPName happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_26 _  = notHappyAtAll 

happyNewToken action sts stk [] =
	action 39 39 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TSkip -> cont 14;
	TStop -> cont 15;
	TPName happy_dollar_dollar -> cont 16;
	TEOName happy_dollar_dollar -> cont 17;
	TEIName happy_dollar_dollar -> cont 18;
	TCExp happy_dollar_dollar -> cont 19;
	TCVar happy_dollar_dollar -> cont 20;
	TPred happy_dollar_dollar -> cont 21;
	TAct happy_dollar_dollar -> cont 22;
	TEvent -> cont 23;
	TFrom -> cont 24;
	TOccurs -> cont 25;
	TWhen -> cont 26;
	TDoes -> cont 27;
	TDef -> cont 28;
	TCIn -> cont 29;
	TCOut -> cont 30;
	TOpen -> cont 31;
	TClose -> cont 32;
	TPrefix -> cont 33;
	TPar -> cont 34;
	TExtSel -> cont 35;
	TIntSel -> cont 36;
	TSeq -> cont 37;
	TInter -> cont 38;
	_ -> happyError' (tk:tks)
	}

happyError_ 39 tk tks = happyError' tks
happyError_ _ tk tks = happyError' (tk:tks)

happyThen :: () => IO a -> (a -> IO b) -> IO b
happyThen = (>>=)
happyReturn :: () => a -> IO a
happyReturn = (return)
happyThen1 m k tks = (>>=) m (\a -> k a tks)
happyReturn1 :: () => a -> b -> IO a
happyReturn1 = \a tks -> (return) a
happyError' :: () => [(Token)] -> IO a
happyError' = parseError

cspparser tks = happySomeParser where
  happySomeParser = happyThen (happyParse action_0 tks) (\x -> case x of {HappyAbsSyn4 z -> happyReturn z; _other -> notHappyAtAll })

happySeq = happyDontSeq


parseError :: [Token] -> a
parseError _ = error "Parse error"

data Token =  TSkip
            | TStop
            | TOpen
            | TClose
            | TPName String
            | TEOName String
            | TEIName String
            | TCExp String
            | TCVar String
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
                                ("STOP", cont) -> TStop : lexer cont
                                ("SKIP", cont) -> TSkip : lexer cont
                                (p, cont)      -> (TPName p):lexer cont
                | isLower c = case fstWord (c:cs) of
                                ("event", cont)  -> TEvent : lexer cont
                                ("from", cont)   -> TFrom : lexer cont
                                ("occurs", cont) -> TOccurs : lexer cont
                                ("when", cont)   -> let (p, contt) = fstWord (dropWhile isSpace cont) in (TWhen : ((TPred p) : lexer contt))
                                ("does", cont)   -> let (a, contt) = fstWord (dropWhile isSpace cont) in (TDoes : ((TAct a) : lexer contt))
                                (e, ('?' : cont))  -> let (v, contt) = fstWord cont in (TEIName e):(TCIn :((TCVar v) : lexer contt))
                                (e, ('!' : cont))  -> let (exp, contt) = paren cont in ((TEIName e):(TCOut :((TCExp exp) : (lexer (tail contt)))))
                                (e, cont)        -> (TEIName e) : lexer cont
                
fstWord = span (\c -> isAlpha c || c == '_' || isDigit c)

paren ('(':rest) = span (\c -> c /= ')') rest           --(takeWhile (\c -> c /= ')') rest) ++ (tail $ dropWhile (\c -> c /= ')') rest)
paren x = span (\c -> c/= ' ') x

lexCom ('-':('}':cs)) = lexer cs
lexCom (c:cs) = lexCom cs
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}







# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4










































{-# LINE 7 "<command-line>" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
-- Id: GenericTemplate.hs,v 1.26 2005/01/14 14:47:22 simonmar Exp 

{-# LINE 13 "templates/GenericTemplate.hs" #-}

{-# LINE 46 "templates/GenericTemplate.hs" #-}








{-# LINE 67 "templates/GenericTemplate.hs" #-}

{-# LINE 77 "templates/GenericTemplate.hs" #-}

{-# LINE 86 "templates/GenericTemplate.hs" #-}

infixr 9 `HappyStk`
data HappyStk a = HappyStk a (HappyStk a)

-----------------------------------------------------------------------------
-- starting the parse

happyParse start_state = happyNewToken start_state notHappyAtAll notHappyAtAll

-----------------------------------------------------------------------------
-- Accepting the parse

-- If the current token is (1), it means we've just accepted a partial
-- parse (a %partial parser).  We must ignore the saved token on the top of
-- the stack in this case.
happyAccept (1) tk st sts (_ `HappyStk` ans `HappyStk` _) =
        happyReturn1 ans
happyAccept j tk st sts (HappyStk ans _) = 
         (happyReturn1 ans)

-----------------------------------------------------------------------------
-- Arrays only: do the next action

{-# LINE 155 "templates/GenericTemplate.hs" #-}

-----------------------------------------------------------------------------
-- HappyState data type (not arrays)



newtype HappyState b c = HappyState
        (Int ->                    -- token number
         Int ->                    -- token number (yes, again)
         b ->                           -- token semantic value
         HappyState b c ->              -- current state
         [HappyState b c] ->            -- state stack
         c)



-----------------------------------------------------------------------------
-- Shifting a token

happyShift new_state (1) tk st sts stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--     trace "shifting the error token" $
     new_state i i tk (HappyState (new_state)) ((st):(sts)) (stk)

happyShift new_state i tk st sts stk =
     happyNewToken new_state ((st):(sts)) ((HappyTerminal (tk))`HappyStk`stk)

-- happyReduce is specialised for the common cases.

happySpecReduce_0 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_0 nt fn j tk st@((HappyState (action))) sts stk
     = action nt j tk st ((st):(sts)) (fn `HappyStk` stk)

happySpecReduce_1 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_1 nt fn j tk _ sts@(((st@(HappyState (action))):(_))) (v1`HappyStk`stk')
     = let r = fn v1 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_2 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_2 nt fn j tk _ ((_):(sts@(((st@(HappyState (action))):(_))))) (v1`HappyStk`v2`HappyStk`stk')
     = let r = fn v1 v2 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happySpecReduce_3 i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happySpecReduce_3 nt fn j tk _ ((_):(((_):(sts@(((st@(HappyState (action))):(_))))))) (v1`HappyStk`v2`HappyStk`v3`HappyStk`stk')
     = let r = fn v1 v2 v3 in
       happySeq r (action nt j tk st sts (r `HappyStk` stk'))

happyReduce k i fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyReduce k nt fn j tk st sts stk
     = case happyDrop (k - ((1) :: Int)) sts of
         sts1@(((st1@(HappyState (action))):(_))) ->
                let r = fn stk in  -- it doesn't hurt to always seq here...
                happyDoSeq r (action nt j tk st1 sts1 r)

happyMonadReduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonadReduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
          let drop_stk = happyDropStk k stk in
          happyThen1 (fn stk tk) (\r -> action nt j tk st1 sts1 (r `HappyStk` drop_stk))

happyMonad2Reduce k nt fn (1) tk st sts stk
     = happyFail (1) tk st sts stk
happyMonad2Reduce k nt fn j tk st sts stk =
      case happyDrop k ((st):(sts)) of
        sts1@(((st1@(HappyState (action))):(_))) ->
         let drop_stk = happyDropStk k stk





             new_state = action

          in
          happyThen1 (fn stk tk) (\r -> happyNewToken new_state sts1 (r `HappyStk` drop_stk))

happyDrop (0) l = l
happyDrop n ((_):(t)) = happyDrop (n - ((1) :: Int)) t

happyDropStk (0) l = l
happyDropStk n (x `HappyStk` xs) = happyDropStk (n - ((1)::Int)) xs

-----------------------------------------------------------------------------
-- Moving to a new state after a reduction

{-# LINE 256 "templates/GenericTemplate.hs" #-}
happyGoto action j tk st = action j j tk (HappyState action)


-----------------------------------------------------------------------------
-- Error recovery ((1) is the error token)

-- parse error if we are in recovery and we fail again
happyFail (1) tk old_st _ stk@(x `HappyStk` _) =
     let i = (case x of { HappyErrorToken (i) -> i }) in
--      trace "failing" $ 
        happyError_ i tk

{-  We don't need state discarding for our restricted implementation of
    "error".  In fact, it can cause some bogus parses, so I've disabled it
    for now --SDM

-- discard a state
happyFail  (1) tk old_st (((HappyState (action))):(sts)) 
                                                (saved_tok `HappyStk` _ `HappyStk` stk) =
--      trace ("discarding state, depth " ++ show (length stk))  $
        action (1) (1) tk (HappyState (action)) sts ((saved_tok`HappyStk`stk))
-}

-- Enter error recovery: generate an error token,
--                       save the old token and carry on.
happyFail  i tk (HappyState (action)) sts stk =
--      trace "entering error recovery" $
        action (1) (1) tk (HappyState (action)) sts ( (HappyErrorToken (i)) `HappyStk` stk)

-- Internal happy errors:

notHappyAtAll :: a
notHappyAtAll = error "Internal Happy error\n"

-----------------------------------------------------------------------------
-- Hack to get the typechecker to accept our action functions







-----------------------------------------------------------------------------
-- Seq-ing.  If the --strict flag is given, then Happy emits 
--      happySeq = happyDoSeq
-- otherwise it emits
--      happySeq = happyDontSeq

happyDoSeq, happyDontSeq :: a -> b -> b
happyDoSeq   a b = a `seq` b
happyDontSeq a b = b

-----------------------------------------------------------------------------
-- Don't inline any functions from the template.  GHC has a nasty habit
-- of deciding to inline happyGoto everywhere, which increases the size of
-- the generated parser quite a bit.

{-# LINE 322 "templates/GenericTemplate.hs" #-}
{-# NOINLINE happyShift #-}
{-# NOINLINE happySpecReduce_0 #-}
{-# NOINLINE happySpecReduce_1 #-}
{-# NOINLINE happySpecReduce_2 #-}
{-# NOINLINE happySpecReduce_3 #-}
{-# NOINLINE happyReduce #-}
{-# NOINLINE happyMonadReduce #-}
{-# NOINLINE happyGoto #-}
{-# NOINLINE happyFail #-}

-- end of Happy Template.
