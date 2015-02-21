module Exec where

import Common
import Data.List
import GhcMonad            (liftIO)
import qualified Language.Haskell.Interpreter as I

execAct :: Act -> Imp -> IO ()
execAct a i = do r <- I.runInterpreter (execAct' a i)
                 case r of
                    Left err -> printInterpreterError err
                    Right () -> return ()
          
execAct' a i = do I.loadModules [i]
                  I.setTopLevelModules [takeWhile (/='.') i]
                  I.setImportsQ [("Prelude", Nothing)]
                  res <- I.interpret a (I.as :: IO ())
                  liftIO res
               
execPred :: Pred -> Imp -> IO Bool
execPred p i = do r <- I.runInterpreter (execPred' p i)
                  case r of
                    Left err -> printInterpreterError err >> return False
                    Right b -> b
                    
execPred' p i = do I.loadModules [i]
                   I.setTopLevelModules [takeWhile (/='.') i]
                   I.setImportsQ [("Prelude", Nothing)]
                   res <- I.interpret p (I.as :: IO Bool)
                   return res
          
evalExp :: Value -> Imp -> IO String
evalExp v i = do r <- I.runInterpreter (evalExp' v i)
                 case r of
                    Left err -> printInterpreterError err >> return ""
                    Right e -> return e
                    
evalExp' v i = do I.loadModules [i]
                  I.setTopLevelModules [takeWhile (/='.') i]
                  I.setImportsQ [("Prelude", Nothing)]
                  exp <- I.eval v
                  return exp
          
say :: String -> I.Interpreter ()
say = liftIO . putStrLn

printInterpreterError :: I.InterpreterError -> IO ()
printInterpreterError e = putStrLn $ "Ups... " ++ (show e)

{-

import Common
import Control.Applicative
import Data.List
import DynFlags
import Unsafe.Coerce
import GHC
import GHC.Paths                        --                                                          cabal install ghc-paths
import GhcMonad            (liftIO)     -- from ghc7.7 and up you can use the usual
                                        -- liftIO from Control.Monad.IO.Class

                                        
                                        

execAct :: Act -> Imp -> IO ()
execAct "" _ = return ()
execAct a file = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        -- we have to call 'setSessionDynFlags' before doing everything else
        dflags <- getSessionDynFlags
        -- If we want to make GHC interpret our code on the fly, we ought to set those two flags, otherwise we wouldn't be able to use 'setContext' below
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [guessTarget file Nothing]
        load LoadAllTargets
        -- Bringing the module into the context
        setContext [IIModule $ mkModuleName (takeWhile (/='.') file)]
        -- evaluating and running an action
        act <- unsafeCoerce <$> compileExpr a           
        liftIO act
        
execPred :: Pred -> Imp -> IO Bool
execPred "" _ = return True
execPred p file = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        -- we have to call 'setSessionDynFlags' before doing everything else
        dflags <- getSessionDynFlags
        -- If we want to make GHC interpret our code on the fly, we ought to set those two flags, otherwise we wouldn't be able to use 'setContext' below
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [guessTarget file Nothing]
        load LoadAllTargets
        -- Bringing the module into the context
        setContext [IIModule $ mkModuleName (takeWhile (/='.') file)]
        -- evaluating and running an action
        act <- unsafeCoerce <$> compileExpr p         
        liftIO act
        
execExp :: Value -> Imp -> IO String
execExp v file = defaultErrorHandler defaultFatalMessager defaultFlushOut $ do
    runGhc (Just libdir) $ do
        -- we have to call 'setSessionDynFlags' before doing everything else
        dflags <- getSessionDynFlags
        -- If we want to make GHC interpret our code on the fly, we ought to set those two flags, otherwise we wouldn't be able to use 'setContext' below
        setSessionDynFlags $ dflags { hscTarget = HscInterpreted
                                    , ghcLink   = LinkInMemory
                                    }
        setTargets =<< sequence [guessTarget file Nothing]
        load LoadAllTargets
        -- Bringing the module into the context
        setContext [IIModule $ mkModuleName (takeWhile (/='.') file)]
        -- evaluating and running an action
        act <- unsafeCoerce <$> compileExpr ("show $ "++v)
        liftIO act
        
   -}        
-- runGhc :: Maybe FilePath -> Ghc a -> IO a
-- getSessionDynFlags :: GhcMonad m => m DynFlags
-- setTargets :: GhcMonad m => [Target] -> m ()
-- guessTarget :: GhcMonad m => String -> Maybe Phase -> m Target
-- load :: GhcMonad m => LoadHowMuch -> m SuccessFlag
-- compileExpr :: GhcMonad m => String -> m HValue
-- unsafeCoerce :: a -> b               The highly unsafe primitive unsafeCoerce converts a value from any type to any other type. 
                                      --Needless to say, if you use this function, it is your responsibility to ensure that the old and new types have identical internal representations, 
                                      --in order to prevent runtime corruption.
-- liftIO :: IO a -> m a 