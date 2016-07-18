{-# OPTIONS -XRecordWildCards #-}

module Main where
    
  import Common
  import ParserCsp
  import Csp
  import Env
  import Exec
  import Data.Char
  import Data.List
  import qualified Data.Set as Set
  import Control.Monad.Random
  import Control.Monad -----------
  import Control.Concurrent
  
-- Installation
-- 	 cabal install monadrandom
--   cabal install hint
--   ghc-pkg expose ghc

  
-- -------------------
-- - Interpreter
-- -------------------

  data Command = LoadSpec String
               | LoadImp String
               | Help
               | Run
               | Quit
               | NoOp
  
  data CmdData = Cmd String String (String -> Command) String
  commands :: [CmdData]
  commands = [Cmd "loadSpec" "<file>" LoadSpec "Carga la especificacion CSP del sistema.",
              Cmd "loadImp" "<file>" LoadImp "Carga la implementacion de los predicados y acciones del sistema.",
              Cmd "run" "" (const Run) "Comienza la ejecucion de la especificacion y la implementacion previamente cargados.",
              Cmd "quit" "" (const Quit) "Cierra el programa.",
              Cmd "help" "" (const Help) "Muestra un texto de ayuda con informacion del programa."]

  data State = S { spec :: Maybe Proc,
                   vars :: PredMap,
                   env :: ProcEnv,
                   imp :: Maybe Imp }
              
  main :: IO ()
  main = let loop st = do putStr ":> "
                          s <- getLine
                          c <- interCmd s
                          st' <- handleCmd st c
                          maybe (return ()) loop st'
         in do putStrLn "BIENVENIDO AL INTERPRETE DE CSP"
               putStrLn "==============================="
               putStrLn ""
               putStrLn "ingrese help para mas ayuda"
               putStrLn ""
               loop (S Nothing envEmpty envEmpty Nothing)
               
               
  interCmd :: String -> IO Command
  interCmd [] = return NoOp
  interCmd str =     let  (cmd,t')  =  break isSpace str
                          t         =  dropWhile isSpace t'
                          matching  =  filter (\ (Cmd cs _ _ _) -> cmd == cs) commands
                     in case matching of
                          [Cmd _ _ f _] ->  return (f t)
                          _             ->  do putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba help para recibir ayuda.")
                                               return NoOp
                       
  handleCmd :: State -> Command -> IO (Maybe State)
  handleCmd st@(S {..}) (LoadSpec file) = do let f'= reverse(dropWhile isSpace (reverse file)) 
                                             f <- readFile f'
                                             (procs, claus) <- (cspparser . lexer) f
                                             b <- chkNames procs
                                             if b then (do predMap <- varsInit claus
                                                           let procs' = setActAndVars procs claus predMap
                                                           defs <- return $ defInit procs'
                                                           sys <- sistema defs
                                                           when debug $ printProc sys           -- sacar
                                                           when debug $ printDefs procs'
                                                           st' <- newSpec sys defs (S spec predMap env imp)
                                                           putStrLn "Specification loaded successfully!"
                                                           return (Just st'))
                                                  else (do putStrLn "Error: nombres repetidos en la definicion de procesos."
                                                           return (Just st)) -- revusar
                                               
                                       
  handleCmd st (LoadImp file) = do putStrLn "Implementation loaded successfully!"
                                   return (Just (newImp file st))
  handleCmd st@(S {..}) Run = maybe (putStrLn "Error: todavia no ha sido cargada la especificacion" >> return (Just st))
                                    (\sist -> maybe (putStrLn "Error: todavia no ha sido cargada la implementacion" >> return (Just st))
                                                    (\impl -> do forkIO (forever (do { updatePreds vars impl}))
                                                                 res <- eval env impl sist
                                                                 st' <- newSpec res env st
                                                                 return (Just st'))															
                                                    imp)
                                    spec
  handleCmd st Quit = putStrLn "Adios!" >> return Nothing
  handleCmd st Help = putStrLn "TODO" >> return (Just st)
  handleCmd st NoOp = return (Just st)
  
     
  newSpec :: Proc -> ProcEnv -> State -> IO State
  newSpec Stop e st@(S {..}) = do putStrLn "Error: especificacion erronea del sistema"
                                  return (S Nothing vars envEmpty imp)
  newSpec p e st@(S {..}) = return (S (Just p) vars e imp)
  
  newImp :: Imp -> State -> State
  newImp file st@(S {..}) = S spec vars env (Just file)


