module Common where

    type Func = () -- IO ()
    data Event = Out String Bool | In String Func

    data Id a = Id a deriving(Show)
    newtype State s a = State { runState :: s -> (a,s) }

    instance Monad Id where
        return x = Id x
        Id x >>= f = f x

    instance Monad (State s) where
        return x = State $ \s -> (x,s)
        (State h) >>= f = State $ \s -> let (a, newState) = h s
                                            (State g) = f a
                                        in  g newState

    instance Ord Event where
        compare (_ s _) (_ s' _) = compare s s'
                                        
    data ProcDef = ProcDef String Proc
    data Proc = Skip | Stop | Ref String | Prefix Event Proc | Parallel [Proc] | ExtSel [Proc]
