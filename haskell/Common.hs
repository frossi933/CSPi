{-# LANGUAGE FlexibleInstances #-}
module Common where

    import qualified Data.Set as Set
    import Control.Concurrent
    import System.IO.Unsafe

    type Pred = String
    type Act = String
    type Id = String
    type Value = String
    type Exp = String
    type BVar = MVar Bool

    initial_state_mvar :: Bool
    initial_state_mvar = False

----------------- DEBUG FLAG
    debug :: Bool
    debug = True

    
    data Event = Eps | E Id (Maybe BVar) (Maybe Act) | C Channel
    data Channel = ComOut Id Value | ComIn Id Id | Com Id Value  deriving(Show) -- ComIn idEvento idVariable, Com es el resultado de la sincronizacion de dos canales...

    data Claus = CPred String String Pred         -- CPred evento proceso predicado
               | CAct String String Act          -- CAcc evento proceso accion

    type EvSet = Set.Set Event
    
    instance Eq Event where
        (==) (Eps) (Eps)          = True
        (==) (E id _ _) (E id' _ _) = id == id'
        (==) (C c) (C c')         = c == c'
        (==) _ _                  = False


    instance Ord Channel where
        compare (ComOut n _) (ComOut n' _) = compare n n'
        compare (ComIn n _) (ComIn n' _)   = compare n n'
        compare (Com n _) (Com n' _)       = compare n n'
        compare c c'                       = LT
        
    instance Eq Channel where
        (==) (ComOut n _) (ComOut n' _) = n == n'
        (==) (ComIn n _) (ComIn n' _)   = n == n'
        (==) (Com n _) (Com n' _)       = n == n'
        (==) c c'                       = False
        
    instance Eq ProcDef where
        (==) (Def s e p) (Def s' e' p') = s == s'
        
    instance Show Event where
        show Eps       = "eps"
        show (E id _ _) = show id
        show (C c)     = show c
 
    instance Ord Event where
        compare v t = compare (nameOfEvent v) (nameOfEvent t)

    instance Show BVar where
        show v = unsafePerformIO ( do { b <- takeMVar v ; putMVar v b ; return $ show b })
              
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
              
    type Imp = String

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

    nameOfCom (ComIn s _)  = s
    nameOfCom (ComOut s _) = s
    nameOfCom (Com s _)    = s

