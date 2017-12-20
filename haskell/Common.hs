{-# LANGUAGE FlexibleInstances #-}
module Common where

    import qualified Data.Set as Set
    import Control.Concurrent
    import System.IO.Unsafe
    import System.IO

    import System.Environment

    type Pred = IO Bool
    type Act = IO ()
    type Func = IO Value
    type Id = String
    type Var = String
    type Value = String
    type Exp = String
    type BVar = MVar Bool

    initial_state_mvar :: Bool
    initial_state_mvar = False

---------- DEBUG FLAG----------
    debug :: Bool
#if DEBUG
    debug = True
#else
    debug = False
#endif
-------------------------------

    type EvSet = Set.Set Event
    data Event = Eps | E Id (Maybe BVar) (Maybe Act) | C Channel
    data Channel = Fun Id String Func | Var Id String | Com Id Value -- Com es el resultado de la sincronizacion de dos canales...

    data Claus = CPred String String String         -- CPred evento proceso predicado
               | CAct String String String          -- CAcc evento proceso accion

    data ProcDef = Def String [Exp] Proc 
    data Proc = Skip 
              | Stop 
              | Ref String [Exp]
              | Prefix Event Proc 
              | Parallel EvSet Proc Proc 
              | ExtSel Proc Proc
              | IntSel Proc Proc
              | Seq Proc Proc
              | Inter Proc Proc



    instance Eq Event where
        (==) (Eps) (Eps)          = True
        (==) (E id _ _) (E id' _ _) = id == id'
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
        
    instance Eq ProcDef where
        (==) (Def s e p) (Def s' e' p') = s == s'
        
    instance Show Event where
        show Eps       = "eps"
        show (E id _ _) = show id
        show (C c)     = show c
 
    instance Show Channel where
        show (Fun id f _) = id++"!"++f
        show (Var id v) = id++"?"++v
        show (Com id _) = "COM:"++id

    instance Ord Event where
        compare v t = compare (nameOfEvent v) (nameOfEvent t)

--    instance Show BVar where
--        show v = unsafePerformIO ( do { b <- takeMVar v ; putMVar v b ; return $ show b })              

    instance Show Proc where
        show Skip = "SKIP"
        show Stop = "STOP"
        show (Ref s e) = (show s) ++ (show e)
        show (Prefix v p) = (show v) ++ " -> " ++ (show p)
        show (Parallel s p q) = "(" ++ (show p) ++ " |{"++ (show s) ++"}| " ++ (show q) ++ ")"
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

