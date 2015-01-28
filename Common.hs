module Common where

    type Pred = String
    type Act = String
    type Id = String
    type Value = String
    
    data Event = Out Id Pred | In Id Act | C Channel
    data Channel = ComOut Id Value | ComIn Id Id | Com Id Value  deriving(Show) -- ComIn idEvento idVariable, Com es el resultado de la sincronizacion de dos canales...

    data Claus = CPred String String Pred         -- CPred evento proceso predicado
               | CAct String String Act          -- CAcc evento proceso accion
    
    
    instance Eq Event where
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
        (==) (Def s p) (Def s' p') = s == s'
        
    instance Show Event where
        show (In s _)  = show s
        show (Out s _) = show s
        show (C c)     = show c
                                        
    data ProcDef = Def String Proc
    data Proc = Skip 
              | Stop 
              | Ref String 
              | Prefix Event Proc 
              | Parallel Proc Proc 
              | ExtSel [Proc]
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