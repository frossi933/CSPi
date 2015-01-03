module Main where
    
  import ParserCsp
  import Csp
  import Data.Char
  import Data.List
  
---------------------
--- Interpreter
---------------------

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

  main :: IO ()
  main = do s <- getLine
            c <- interCmd s
            handleCmd c
            
  interCmd :: String -> IO Command
  interCmd [] = return NoOp
  interCmd str =     let  (cmd,t')  =  break isSpace str
                          t         =  dropWhile isSpace t'
                          matching  =  filter (\ (Cmd cs _ _ _) -> cmd == cs) commands
                     in case matching of
                          [Cmd _ _ f _] ->  return (f t)
                          _             ->  do putStrLn ("Comando desconocido `" ++ cmd ++ "'. Escriba help para recibir ayuda.")
                                               return NoOp
                       
  handleCmd :: Command -> IO ()
  handleCmd (LoadSpec file) = let f'= reverse(dropWhile isSpace (reverse file)) 
                              in do f <- readFile f'
                                    printDefs $ (cspparser . lexer) f    
  handleCmd (LoadImp file) = putStrLn "TODO"
  handleCmd Run = putStrLn "TODO"
  handleCmd Quit = putStrLn "TODO"
  handleCmd Help = putStrLn "TODO"
  handleCmd NoOp = putStrLn "TODO"
      
--  parseIO :: String -> (String -> ParseResult a) -> String -> IO (Maybe a)
--  parseIO f p x = case p x of
--                       Failed e  -> do putStrLn (f++": "++e) 
--                                       return Nothing
--                       Ok r      -> return (Just r)