{-# OPTIONS_GHC -w #-}
module ParserCsp where

import Common
import Csp
import Data.Char
import qualified Data.Set as Set
import Control.Concurrent
import System.IO.Unsafe
import Control.Applicative(Applicative(..))
import Control.Monad (ap)

-- parser produced by Happy Version 1.19.5

data HappyAbsSyn t4 t5 t6 t7 t8 t9 t10 t11 t12 t13 t14 t15
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
	| HappyAbsSyn14 t14
	| HappyAbsSyn15 t15

action_0 (18) = happyShift action_4
action_0 (4) = happyGoto action_5
action_0 (7) = happyGoto action_6
action_0 (8) = happyGoto action_3
action_0 _ = happyFail

action_1 (18) = happyShift action_4
action_1 (7) = happyGoto action_2
action_1 (8) = happyGoto action_3
action_1 _ = happyFail

action_2 _ = happyFail

action_3 (18) = happyShift action_4
action_3 (7) = happyGoto action_12
action_3 (8) = happyGoto action_3
action_3 _ = happyReduce_7

action_4 (30) = happyShift action_10
action_4 (33) = happyShift action_11
action_4 _ = happyFail

action_5 (43) = happyAccept
action_5 _ = happyFail

action_6 (25) = happyShift action_9
action_6 (5) = happyGoto action_7
action_6 (6) = happyGoto action_8
action_6 _ = happyReduce_1

action_7 _ = happyReduce_2

action_8 (25) = happyShift action_9
action_8 (5) = happyGoto action_27
action_8 (6) = happyGoto action_8
action_8 _ = happyReduce_3

action_9 (19) = happyShift action_26
action_9 _ = happyFail

action_10 (16) = happyShift action_20
action_10 (17) = happyShift action_21
action_10 (18) = happyShift action_22
action_10 (19) = happyShift action_23
action_10 (20) = happyShift action_24
action_10 (33) = happyShift action_25
action_10 (10) = happyGoto action_15
action_10 (11) = happyGoto action_16
action_10 (13) = happyGoto action_17
action_10 (14) = happyGoto action_18
action_10 (15) = happyGoto action_19
action_10 _ = happyFail

action_11 (21) = happyShift action_14
action_11 (9) = happyGoto action_13
action_11 _ = happyFail

action_12 _ = happyReduce_8

action_13 (34) = happyShift action_40
action_13 _ = happyFail

action_14 (35) = happyShift action_39
action_14 _ = happyReduce_11

action_15 (37) = happyShift action_34
action_15 (39) = happyShift action_35
action_15 (40) = happyShift action_36
action_15 (41) = happyShift action_37
action_15 (42) = happyShift action_38
action_15 _ = happyReduce_9

action_16 (36) = happyShift action_33
action_16 _ = happyFail

action_17 (31) = happyShift action_31
action_17 (32) = happyShift action_32
action_17 _ = happyReduce_23

action_18 _ = happyReduce_24

action_19 _ = happyReduce_17

action_20 _ = happyReduce_14

action_21 _ = happyReduce_13

action_22 (33) = happyShift action_30
action_22 _ = happyReduce_32

action_23 _ = happyReduce_31

action_24 _ = happyReduce_30

action_25 (16) = happyShift action_20
action_25 (17) = happyShift action_21
action_25 (18) = happyShift action_22
action_25 (19) = happyShift action_23
action_25 (20) = happyShift action_24
action_25 (33) = happyShift action_25
action_25 (10) = happyGoto action_29
action_25 (11) = happyGoto action_16
action_25 (13) = happyGoto action_17
action_25 (14) = happyGoto action_18
action_25 (15) = happyGoto action_19
action_25 _ = happyFail

action_26 (26) = happyShift action_28
action_26 _ = happyFail

action_27 _ = happyReduce_4

action_28 (18) = happyShift action_54
action_28 _ = happyFail

action_29 (34) = happyShift action_53
action_29 (37) = happyShift action_34
action_29 (39) = happyShift action_35
action_29 (40) = happyShift action_36
action_29 (41) = happyShift action_37
action_29 (42) = happyShift action_38
action_29 _ = happyFail

action_30 (21) = happyShift action_14
action_30 (9) = happyGoto action_52
action_30 _ = happyFail

action_31 (22) = happyShift action_51
action_31 _ = happyFail

action_32 (21) = happyShift action_50
action_32 _ = happyFail

action_33 (16) = happyShift action_20
action_33 (17) = happyShift action_21
action_33 (18) = happyShift action_22
action_33 (19) = happyShift action_23
action_33 (20) = happyShift action_24
action_33 (33) = happyShift action_25
action_33 (10) = happyGoto action_49
action_33 (11) = happyGoto action_16
action_33 (13) = happyGoto action_17
action_33 (14) = happyGoto action_18
action_33 (15) = happyGoto action_19
action_33 _ = happyFail

action_34 (19) = happyShift action_23
action_34 (20) = happyShift action_24
action_34 (11) = happyGoto action_47
action_34 (12) = happyGoto action_48
action_34 (13) = happyGoto action_17
action_34 (14) = happyGoto action_18
action_34 _ = happyReduce_27

action_35 (16) = happyShift action_20
action_35 (17) = happyShift action_21
action_35 (18) = happyShift action_22
action_35 (19) = happyShift action_23
action_35 (20) = happyShift action_24
action_35 (33) = happyShift action_25
action_35 (10) = happyGoto action_46
action_35 (11) = happyGoto action_16
action_35 (13) = happyGoto action_17
action_35 (14) = happyGoto action_18
action_35 (15) = happyGoto action_19
action_35 _ = happyFail

action_36 (16) = happyShift action_20
action_36 (17) = happyShift action_21
action_36 (18) = happyShift action_22
action_36 (19) = happyShift action_23
action_36 (20) = happyShift action_24
action_36 (33) = happyShift action_25
action_36 (10) = happyGoto action_45
action_36 (11) = happyGoto action_16
action_36 (13) = happyGoto action_17
action_36 (14) = happyGoto action_18
action_36 (15) = happyGoto action_19
action_36 _ = happyFail

action_37 (16) = happyShift action_20
action_37 (17) = happyShift action_21
action_37 (18) = happyShift action_22
action_37 (19) = happyShift action_23
action_37 (20) = happyShift action_24
action_37 (33) = happyShift action_25
action_37 (10) = happyGoto action_44
action_37 (11) = happyGoto action_16
action_37 (13) = happyGoto action_17
action_37 (14) = happyGoto action_18
action_37 (15) = happyGoto action_19
action_37 _ = happyFail

action_38 (16) = happyShift action_20
action_38 (17) = happyShift action_21
action_38 (18) = happyShift action_22
action_38 (19) = happyShift action_23
action_38 (20) = happyShift action_24
action_38 (33) = happyShift action_25
action_38 (10) = happyGoto action_43
action_38 (11) = happyGoto action_16
action_38 (13) = happyGoto action_17
action_38 (14) = happyGoto action_18
action_38 (15) = happyGoto action_19
action_38 _ = happyFail

action_39 (21) = happyShift action_14
action_39 (9) = happyGoto action_42
action_39 _ = happyFail

action_40 (30) = happyShift action_41
action_40 _ = happyFail

action_41 (16) = happyShift action_20
action_41 (17) = happyShift action_21
action_41 (18) = happyShift action_22
action_41 (19) = happyShift action_23
action_41 (20) = happyShift action_24
action_41 (33) = happyShift action_25
action_41 (10) = happyGoto action_60
action_41 (11) = happyGoto action_16
action_41 (13) = happyGoto action_17
action_41 (14) = happyGoto action_18
action_41 (15) = happyGoto action_19
action_41 _ = happyFail

action_42 _ = happyReduce_12

action_43 (37) = happyShift action_34
action_43 _ = happyReduce_22

action_44 (37) = happyShift action_34
action_44 _ = happyReduce_21

action_45 (37) = happyShift action_34
action_45 _ = happyReduce_20

action_46 (37) = happyShift action_34
action_46 _ = happyReduce_19

action_47 (35) = happyShift action_59
action_47 _ = happyReduce_28

action_48 (38) = happyShift action_58
action_48 _ = happyFail

action_49 _ = happyReduce_16

action_50 _ = happyReduce_26

action_51 _ = happyReduce_25

action_52 (34) = happyShift action_57
action_52 _ = happyFail

action_53 _ = happyReduce_15

action_54 (27) = happyShift action_55
action_54 (29) = happyShift action_56
action_54 _ = happyFail

action_55 (28) = happyShift action_64
action_55 _ = happyFail

action_56 (24) = happyShift action_63
action_56 _ = happyFail

action_57 _ = happyReduce_33

action_58 (16) = happyShift action_20
action_58 (17) = happyShift action_21
action_58 (18) = happyShift action_22
action_58 (19) = happyShift action_23
action_58 (20) = happyShift action_24
action_58 (33) = happyShift action_25
action_58 (10) = happyGoto action_62
action_58 (11) = happyGoto action_16
action_58 (13) = happyGoto action_17
action_58 (14) = happyGoto action_18
action_58 (15) = happyGoto action_19
action_58 _ = happyFail

action_59 (19) = happyShift action_23
action_59 (20) = happyShift action_24
action_59 (11) = happyGoto action_47
action_59 (12) = happyGoto action_61
action_59 (13) = happyGoto action_17
action_59 (14) = happyGoto action_18
action_59 _ = happyReduce_27

action_60 (37) = happyShift action_34
action_60 (39) = happyShift action_35
action_60 (40) = happyShift action_36
action_60 (41) = happyShift action_37
action_60 (42) = happyShift action_38
action_60 _ = happyReduce_10

action_61 _ = happyReduce_29

action_62 _ = happyReduce_18

action_63 _ = happyReduce_6

action_64 (23) = happyShift action_65
action_64 _ = happyFail

action_65 _ = happyReduce_5

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
	(HappyTerminal (TEOName happy_var_2)) `HappyStk`
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
	(HappyTerminal (TEOName happy_var_2)) `HappyStk`
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
happyReduction_9 (HappyAbsSyn10  happy_var_3)
	_
	(HappyTerminal (TPName happy_var_1))
	 =  HappyAbsSyn8
		 (Def happy_var_1 [] happy_var_3
	)
happyReduction_9 _ _ _  = notHappyAtAll 

happyReduce_10 = happyReduce 6 8 happyReduction_10
happyReduction_10 ((HappyAbsSyn10  happy_var_6) `HappyStk`
	_ `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TPName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn8
		 (Def happy_var_1 happy_var_3 happy_var_6
	) `HappyStk` happyRest

happyReduce_11 = happySpecReduce_1  9 happyReduction_11
happyReduction_11 (HappyTerminal (TExp happy_var_1))
	 =  HappyAbsSyn9
		 ([happy_var_1]
	)
happyReduction_11 _  = notHappyAtAll 

happyReduce_12 = happySpecReduce_3  9 happyReduction_12
happyReduction_12 (HappyAbsSyn9  happy_var_3)
	_
	(HappyTerminal (TExp happy_var_1))
	 =  HappyAbsSyn9
		 (happy_var_1:happy_var_3
	)
happyReduction_12 _ _ _  = notHappyAtAll 

happyReduce_13 = happySpecReduce_1  10 happyReduction_13
happyReduction_13 _
	 =  HappyAbsSyn10
		 (Stop
	)

happyReduce_14 = happySpecReduce_1  10 happyReduction_14
happyReduction_14 _
	 =  HappyAbsSyn10
		 (Skip
	)

happyReduce_15 = happySpecReduce_3  10 happyReduction_15
happyReduction_15 _
	(HappyAbsSyn10  happy_var_2)
	_
	 =  HappyAbsSyn10
		 (happy_var_2
	)
happyReduction_15 _ _ _  = notHappyAtAll 

happyReduce_16 = happySpecReduce_3  10 happyReduction_16
happyReduction_16 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn10
		 (Prefix happy_var_1 happy_var_3
	)
happyReduction_16 _ _ _  = notHappyAtAll 

happyReduce_17 = happySpecReduce_1  10 happyReduction_17
happyReduction_17 (HappyAbsSyn15  happy_var_1)
	 =  HappyAbsSyn10
		 (happy_var_1
	)
happyReduction_17 _  = notHappyAtAll 

happyReduce_18 = happyReduce 5 10 happyReduction_18
happyReduction_18 ((HappyAbsSyn10  happy_var_5) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn12  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyAbsSyn10  happy_var_1) `HappyStk`
	happyRest)
	 = HappyAbsSyn10
		 (Parallel happy_var_3 happy_var_1 happy_var_5
	) `HappyStk` happyRest

happyReduce_19 = happySpecReduce_3  10 happyReduction_19
happyReduction_19 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (ExtSel happy_var_1 happy_var_3
	)
happyReduction_19 _ _ _  = notHappyAtAll 

happyReduce_20 = happySpecReduce_3  10 happyReduction_20
happyReduction_20 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (IntSel happy_var_1 happy_var_3
	)
happyReduction_20 _ _ _  = notHappyAtAll 

happyReduce_21 = happySpecReduce_3  10 happyReduction_21
happyReduction_21 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Seq happy_var_1 happy_var_3
	)
happyReduction_21 _ _ _  = notHappyAtAll 

happyReduce_22 = happySpecReduce_3  10 happyReduction_22
happyReduction_22 (HappyAbsSyn10  happy_var_3)
	_
	(HappyAbsSyn10  happy_var_1)
	 =  HappyAbsSyn10
		 (Inter happy_var_1 happy_var_3
	)
happyReduction_22 _ _ _  = notHappyAtAll 

happyReduce_23 = happySpecReduce_1  11 happyReduction_23
happyReduction_23 (HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (In happy_var_1 (unsafePerformIO (do { v <- newEmptyMVar :: IO (MVar Bool) ; putMVar v True ; return v }))
	)
happyReduction_23 _  = notHappyAtAll 

happyReduce_24 = happySpecReduce_1  11 happyReduction_24
happyReduction_24 (HappyAbsSyn14  happy_var_1)
	 =  HappyAbsSyn11
		 (Out happy_var_1 ""
	)
happyReduction_24 _  = notHappyAtAll 

happyReduce_25 = happySpecReduce_3  11 happyReduction_25
happyReduction_25 (HappyTerminal (TVar happy_var_3))
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (C (ComIn happy_var_1 happy_var_3)
	)
happyReduction_25 _ _ _  = notHappyAtAll 

happyReduce_26 = happySpecReduce_3  11 happyReduction_26
happyReduction_26 (HappyTerminal (TExp happy_var_3))
	_
	(HappyAbsSyn13  happy_var_1)
	 =  HappyAbsSyn11
		 (C (ComOut happy_var_1 happy_var_3)
	)
happyReduction_26 _ _ _  = notHappyAtAll 

happyReduce_27 = happySpecReduce_0  12 happyReduction_27
happyReduction_27  =  HappyAbsSyn12
		 (Set.empty
	)

happyReduce_28 = happySpecReduce_1  12 happyReduction_28
happyReduction_28 (HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (Set.singleton happy_var_1
	)
happyReduction_28 _  = notHappyAtAll 

happyReduce_29 = happySpecReduce_3  12 happyReduction_29
happyReduction_29 (HappyAbsSyn12  happy_var_3)
	_
	(HappyAbsSyn11  happy_var_1)
	 =  HappyAbsSyn12
		 (Set.insert happy_var_1 happy_var_3
	)
happyReduction_29 _ _ _  = notHappyAtAll 

happyReduce_30 = happySpecReduce_1  13 happyReduction_30
happyReduction_30 (HappyTerminal (TEIName happy_var_1))
	 =  HappyAbsSyn13
		 (happy_var_1
	)
happyReduction_30 _  = notHappyAtAll 

happyReduce_31 = happySpecReduce_1  14 happyReduction_31
happyReduction_31 (HappyTerminal (TEOName happy_var_1))
	 =  HappyAbsSyn14
		 (happy_var_1
	)
happyReduction_31 _  = notHappyAtAll 

happyReduce_32 = happySpecReduce_1  15 happyReduction_32
happyReduction_32 (HappyTerminal (TPName happy_var_1))
	 =  HappyAbsSyn15
		 (Ref happy_var_1 []
	)
happyReduction_32 _  = notHappyAtAll 

happyReduce_33 = happyReduce 4 15 happyReduction_33
happyReduction_33 (_ `HappyStk`
	(HappyAbsSyn9  happy_var_3) `HappyStk`
	_ `HappyStk`
	(HappyTerminal (TPName happy_var_1)) `HappyStk`
	happyRest)
	 = HappyAbsSyn15
		 (Ref happy_var_1 happy_var_3
	) `HappyStk` happyRest

happyNewToken action sts stk [] =
	action 43 43 notHappyAtAll (HappyState action) sts stk []

happyNewToken action sts stk (tk:tks) =
	let cont i = action i i tk (HappyState action) sts stk tks in
	case tk of {
	TSkip -> cont 16;
	TStop -> cont 17;
	TPName happy_dollar_dollar -> cont 18;
	TEOName happy_dollar_dollar -> cont 19;
	TEIName happy_dollar_dollar -> cont 20;
	TExp happy_dollar_dollar -> cont 21;
	TVar happy_dollar_dollar -> cont 22;
	TPred happy_dollar_dollar -> cont 23;
	TAct happy_dollar_dollar -> cont 24;
	TEvent -> cont 25;
	TFrom -> cont 26;
	TOccurs -> cont 27;
	TWhen -> cont 28;
	TDoes -> cont 29;
	TDef -> cont 30;
	TCIn -> cont 31;
	TCOut -> cont 32;
	TOpen -> cont 33;
	TClose -> cont 34;
	TComma -> cont 35;
	TPrefix -> cont 36;
	TParOpen -> cont 37;
	TParClose -> cont 38;
	TExtSel -> cont 39;
	TIntSel -> cont 40;
	TSeq -> cont 41;
	TInter -> cont 42;
	_ -> happyError' (tk:tks)
	}

happyError_ 43 tk tks = happyError' tks
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
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "templates/GenericTemplate.hs" #-}
{-# LINE 1 "<built-in>" #-}
{-# LINE 1 "<command-line>" #-}
{-# LINE 8 "<command-line>" #-}
# 1 "/usr/include/stdc-predef.h" 1 3 4

# 17 "/usr/include/stdc-predef.h" 3 4















































{-# LINE 8 "<command-line>" #-}
{-# LINE 1 "/usr/lib/ghc-7.10.3/include/ghcversion.h" #-}

















{-# LINE 8 "<command-line>" #-}
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
