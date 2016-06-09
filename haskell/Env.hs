module Env where

    import qualified Data.Map as Map
    import Common
    
    type Env = Map.Map String Proc

    envInsert :: String -> Proc -> Env -> Env
    envInsert = Map.insert
    
    envLookup :: String -> Env -> Maybe Proc
    envLookup = Map.lookup
    
    envGetRef :: String -> Exp -> Env -> Maybe Proc
    envGetRef name exp env = case Map.lookup name env of
                                  Just p -> Just p  -- SEGUIRRRR    sustProc 
                                  Nothing -> Nothing
    
    envEmpty :: Env
    envEmpty = Map.empty
    
    envInit :: [ProcDef] -> Env
    envInit ds = foldl (\e (Def name _ p) -> envInsert name p e) envEmpty ds
