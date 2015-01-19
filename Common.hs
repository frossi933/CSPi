module Common where

--    import Actions
    type Pred = String
    type Act = String
    data Event = Out String Pred | In String Act

    data Claus = CPred String String Pred         -- CPred evento proceso predicado
               | CAct String String Act          -- CAcc evento proceso accion
    
    instance Eq Event where
        (==) (In s _) (In s' _) = s == s'
        (==) (In s _) (Out s' _) = s == s'
        (==) (Out s _) (In s' _) = s == s'
        (==) (Out s _) (Out s' _) = s == s'
    
    instance Ord Event where
        compare (In s _) (In s' _) = compare s s'
        compare (In s _) (Out s' _) = compare s s'
        compare (Out s _) (In s' _) = compare s s'
        compare (Out s _) (Out s' _) = compare s s'
        
    instance Eq ProcDef where
        (==) (Def s p) (Def s' p') = s == s'
        
    instance Show Event where
        show (In s _) = show s
        show (Out s _) = show s
                                        
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