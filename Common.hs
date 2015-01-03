{-# LANGUAGE ExistentialQuantification #-}

module Common where

    import Data.List
--    data Func where
--        F :: Monad m => m () -> Func
    type Func = IO ()    
    data Event = Out String Bool | In String Func

    instance Eq Event where
        (==) e e' = strOf e == strOf e'
        
    
    data ProcDef = ProcDef String Proc
--    data Proc = Skip | Stop | Ref String | Prefix Event Proc | Parallel [Proc] | ExtSel [Proc]
    data Proc = Skip | Stop | P (Event -> Proc)
    
    
    strOf :: Event -> String
    strOf (Out s _) = s
    strOf (In s _) = s
    
    skip :: Proc
    skip = choice [] -- por ahora
    
    stop :: Proc
    stop = choice []
    
    prefix :: Event ->  Proc -> Proc
    prefix e p = choice [(e,p)]
                                 
    choice :: [(Event,Proc)] -> Proc
    choice [] = Stop
    choice (x:xs) = if chkEv (x:xs) then P (\e' -> maybe Stop snd (find (\(ee,pp) -> e' == ee) (x:xs)))
                                    else Stop
                                            where chkEv [] = True
                                                  chkEv ((e, p):xs) = case find (\(e', p') -> e == e') xs of
                                                                                    Just _-> False
                                                                                    Nothing -> chkEv xs