module Common where

    type Func = () -- IO ()
    data Event = Out String Bool | In String Func

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
                                        
    data ProcDef = Def String Proc
    data Proc = Skip 
              | Stop 
              | Ref String Proc 
              | Prefix Event Proc 
              | Parallel Proc Proc 
              | ExtSel [Proc]
    data Imp = I