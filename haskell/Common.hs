module Common where

    import qualified Data.Set as Set

    type Pred = String
    type Act = String
    type Id = String
    type Value = String
    type Exp = String
    
    data Event = Eps | In Id Pred | Out Id Act | C Channel
    data Channel = ComOut Id Value | ComIn Id Id | Com Id Value  deriving(Show) -- ComIn idEvento idVariable, Com es el resultado de la sincronizacion de dos canales...

    data Claus = CPred String String Pred         -- CPred evento proceso predicado
               | CAct String String Act          -- CAcc evento proceso accion

    type EvSet = Set.Set Event
    
    instance Eq Event where
        (==) (Eps) (Eps)          = True
        (==) (Out n _) (Out n' _) = n == n'
        (==) (In n _) (In n' _)   = n == n'
        (==) (Out n _) (In n' _)  = n == n'
        (==) (In n _) (Out n' _)  = n == n'
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
        show (In s _)  = "-" ++ show s
        show (Out s _) = show s
        show (C c)     = show c
 
    instance Ord Event where
        compare v t = compare (nameOfEvent v) (nameOfEvent t)

                                       
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

    nameOfEvent (In s _)   = s
    nameOfEvent (Out s _)  = s
    nameOfEvent (C c)      = nameOfCom c
    nameOfEvent Eps        = "eps"

    nameOfCom (ComIn s _)  = s
    nameOfCom (ComOut s _) = s
    nameOfCom (Com s _)    = s

