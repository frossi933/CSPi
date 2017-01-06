{-# LANGUAGE TemplateHaskell #-}
module Env where

    import qualified Data.Map as Map
    import Data.String
    import Common
    import AddTopDecls
    import Imp

    import Language.Haskell.TH  (dyn,listE)
    import Control.Concurrent
  
    type ProcEnv = Map.Map String Proc
    type PredMap = Map.Map String (Pred, BVar)
    type ActMap = Map.Map String Act
    type FunMap = Map.Map String Func


    envInsert = Map.insert
    
    
    envGetRef :: String -> [Exp] -> ProcEnv -> Maybe Proc
    envGetRef name exp env = case Map.lookup name env of
                                  Just p -> Just p  -- SEGUIRRRR    sustProc 
                                  Nothing -> Nothing

    envGetVar st env = maybe Nothing (Just . snd) (Map.lookup st env)

    envGetAct st env = Map.lookup st env

    envGetFun st env = Map.lookup st env
    
    envEmpty = Map.empty

    envElems :: PredMap -> [(Pred, BVar)]
    envElems = Map.elems
    
    defInit :: [ProcDef] -> ProcEnv
    defInit ds = foldl (\e (Def name _ p) -> envInsert name p e) envEmpty ds


    actInit :: IO ActMap
    actInit = do let actList' = $(listE (map (dyn . snd) actNames))
                 let actList = zip (map fst actNames) actList'
                 return $ Map.fromList actList

    predInit :: IO PredMap
    predInit = do let predList' = $(listE (map (dyn . snd) predNames))
                  let predList = zip (map fst predNames) predList'
                  predInit' predList envEmpty                     

    predInit' [] env = return env
    predInit' ((event, pred) : fs) env = do mvar <- newEmptyMVar
                                            putMVar mvar initial_state_mvar
                                            predInit' fs (envInsert event (pred, mvar) env)

    funsInit :: IO FunMap
    funsInit = do let funsList' = $(listE (map (dyn . snd) funNames))
                  let funList = zip (map fst funNames) funList'
                  return $ Map.fromList funList

