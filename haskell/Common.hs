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
        (==) (Out _ _) (C _)      = False
        (==) (In _ _) (C _)       = False
        (==) (C _) (Out _ _)      = False
        (==) (C _) (In _ _)       = False
        (==) (C c) (C c')         = c == c'

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
        
    instance Ord Event where
        compare e e' = compare (nameOfEvent e) (nameOfEvent e')
        
    instance Eq ProcDef where
        (==) (Def s e p) (Def s' e' p') = s == s'
        
    instance Show Event where
        show (In s _)  = show s
        show (Out s _) = show s
        show (C c)     = show c
                                        
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

    nameOfEvent (In s _)   = s
    nameOfEvent (Out s _)  = s
    nameOfEvent (C c)      = nameOfCom c

    nameOfCom (ComIn s _)  = s
    nameOfCom (ComOut s _) = s
    nameOfCom (Com s _)    = s
