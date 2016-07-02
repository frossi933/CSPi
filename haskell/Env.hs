module Env where

    import qualified Data.Map as Map
    import Common
    import Control.Concurrent
    
    type ProcEnv = Map.Map String Proc
    type PredMap = Map.Map String (Pred, MVar Bool)



--    envInsert :: String -> Proc -> Env -> Env
    envInsert = Map.insert
    
--    envLookup :: String -> Env -> Maybe Proc
--    envLookup = Map.lookup
    
    envGetRef :: String -> [Exp] -> ProcEnv -> Maybe Proc
    envGetRef name exp env = case Map.lookup name env of
                                  Just p -> Just p  -- SEGUIRRRR    sustProc 
                                  Nothing -> Nothing

    envGetVar st env = maybe Nothing (Just . snd) (Map.lookup st env)
    
--    envEmpty :: Env
    envEmpty = Map.empty

    envElems = Map.elems
    
    defInit :: [ProcDef] -> ProcEnv
    defInit ds = foldl (\e (Def name _ p) -> envInsert name p e) envEmpty ds

    varsInit :: [Claus] -> IO PredMap
    varsInit cs = varsInit' cs envEmpty

    varsInit' [] env = return env
    varsInit' ((CPred event proc pred) : cs) env = do mvar <- newEmptyMVar
                                                      putMVar mvar initial_state_mvar
                                                      varsInit' cs (envInsert event (pred, mvar) env)
    varsInit' (_ : cs) env = varsInit' cs env
