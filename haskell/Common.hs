{-# LANGUAGE FlexibleInstances #-}
module Common where

    import Prelude hiding( (&&) )
    type Func = IO Value
    type Id = String
    type Var = String
    type Value = String
    type Cond = IO Bool
    type Action = IO ()

---------- DEBUG FLAG----------
    debug :: Bool
    debug = False
{- #if DEBUG
    debug = True
#else
    debug = False
#endif-}
-------------------------------

    data Event = Eps
               | E Id (Maybe Cond) (Maybe Action)
               | C Channel
    data Channel = Fun Id String Func | Var Id String | Com Id Value -- Com es el resultado de la sincronizacion de dos canales...

    data Proc = Skip
              | Stop
              | Ref Proc
              | Prefix Event Proc
              | Parallel Proc Proc
              | ExtSel Proc Proc
              | IntSel Proc Proc
              | Seq Proc Proc
              | Inter Proc Proc

    newEvent :: String -> Event
    newEvent name = E name Nothing Nothing

    newAction :: String -> Action -> Event
    newAction name func = E name Nothing (Just func)

    newCondition :: String -> Cond -> Event
    newCondition name cond = E name (Just cond) Nothing


    infixl 5 -->
    (-->) :: Event -> Proc -> Proc
    e --> p = Prefix e p

    (|||) :: Proc -> Proc -> Proc
    p ||| q = Parallel p q

    (><) :: Proc -> Proc -> Proc
    p >< q = ExtSel p q

    (<>) :: Proc -> Proc -> Proc
    p <> q = IntSel p q

    (!>) :: Proc -> Proc -> Proc
    p !> q = Inter p q

    (...) :: Proc -> Proc -> Proc
    p ... q = Seq p q

    instance Eq Event where
        (==) (Eps) (Eps)          = True
        (==) (E id' _ _) (E id'' _ _) = id' == id''
        (==) (C c) (C c')         = c == c'
        (==) _ _                  = False

    instance Ord Channel where
        compare (Fun n _ _) (Fun n' _ _) = compare n n'
        compare (Var n _) (Var n' _)   = compare n n'
        compare (Com n _) (Com n' _)       = compare n n'
        compare c c'                       = LT

    instance Eq Channel where
        (==) (Fun n _ _) (Fun n' _ _) = n == n'
        (==) (Var n _) (Var n' _)   = n == n'
        (==) (Com n _) (Com n' _)       = n == n'
        (==) c c'                       = False

    instance Show Cond where -- FIXME
        show c = "cond"

    instance Show Action where -- FIXME
        show a = "action"

    instance Show Event where
        show Eps       = "eps"
        show (E id' c a) = show id' ++ " " ++ show c ++ " " ++ show a
        show (C c)     = show c

    instance Show Channel where
        show (Fun id' f _) = id'++"!"++f
        show (Var id' v) = id'++"?"++v
        show (Com id' _) = "COM:"++id'

    instance Ord Event where
        compare v t = compare (nameOfEvent v) (nameOfEvent t)

    instance Show Proc where
        show Skip = "SKIP"
        show Stop = "STOP"
        show (Ref p) = "REF"
        show (Prefix v p) = (show v) ++ " -> " ++ (show p)
        show (Parallel p q) = "(" ++ (show p) ++ (show q) ++ ")"
        show (ExtSel p q) = (show p) ++ " [] " ++ (show q)
        show (IntSel p q) = (show p) ++ " /] " ++ (show q)
        show (Seq p q) = (show p) ++ " ; " ++ (show q)
        show (Inter p q) = (show p) ++ " |> " ++ (show q)

    nameOfEvent (E e _ _) = e
    nameOfEvent (C c)     = nameOfCom c
    nameOfEvent Eps       = "eps"

    nameOfCom (Fun s _ _)  = s
    nameOfCom (Var s _) = s
    nameOfCom (Com s _)    = s
