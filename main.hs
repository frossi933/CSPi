{-# OPTIONS -XRecordWildCards #-}

module Main where
    
  import Common
  import ParserCsp
  import Csp
  import Env
  import Data.Char
  import Data.List
  import qualified Data.Set as Set 
  
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
                   env :: Env,
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
               loop (S Nothing envEmpty Nothing)
               
               
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
  handleCmd st (LoadSpec file) = let f'= reverse(dropWhile isSpace (reverse file)) 
                                 in do f <- readFile f'
                                       (defs, claus) <- (cspparser . lexer) f
                                       b <- chkNames defs
                                       if b then (let defs' = setPredAct defs claus in do 
                                                     env <- return $ envInit defs'
                                                     sys <- sistema env
                                                     printProc sys           -- sacar
                                                     st' <- newSpec sys env st
                                                     return (Just st'))
                                            else (do putStrLn "Error: nombres repetidos en la definicion de procesos."
                                                     return (Just st)) -- revusar
                                       
                                       
  handleCmd st (LoadImp file) = return (Just (newImp file st))
  handleCmd st@(S {..}) Run = maybe (putStrLn "Error: todavia no ha sido cargada la especificacion" >> return (Just st))
                                    (\sist -> do m <- return $ menu sist env
                                                 print m                                        -- sacar
                                                 e <- return $ Set.elemAt ((Set.size m)- 1) m    -- debe ser random
                                                 print e                                            -- sacar
                                                 sist' <- return $ eval sist e env
                                                 printProc sist'
                                                 st' <- newSpec sist' env st
                                                 return (Just st'))
                                    spec
  handleCmd st Quit = putStrLn "Adios!" >> return Nothing
  handleCmd st Help = putStrLn "TODO" >> return (Just st)
  handleCmd st NoOp = return (Just st)
      
  newSpec :: Proc -> Env -> State -> IO State
  newSpec Stop e st@(S {..}) = do putStrLn "Error: especificacion erronea del sistema"
                                  return (S Nothing envEmpty imp)
  newSpec p e st@(S {..}) = return (S (Just p) e imp)
  
  newImp :: Imp -> State -> State
  newImp file st@(S {..}) = S spec env (Just file)
--  parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
--  parseIO f p x = case p x of
--                       Failed e  -> do putStrLn (f++": "++e) 
--                                       return Nothing
--                       Ok r      -> return (Just r)