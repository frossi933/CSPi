module Main where

    import Common
    import Csp
    import ParserCsp
    import Env
    import Control.Concurrent
    import System.Environment
    import Control.Monad
    import Data.Char

    main :: IO ()
    main = do args <- getArgs
              let specPath = args !! 0
              let impPath = args !! 1
              (procs, claus) <- parseSpec specPath
              namesOk <- chkNames procs
              when namesOk (do predMap <- predInit
                               actMap <- actInit
                               let procs' = setActAndVars procs claus actMap predMap
                               defs <- return $ defInit procs'
                               sys <- sistema defs
                               when debug $ do { printProc sys ; printDefs procs' ; putStrLn "Specification loaded successfully!" }
                               tid <- forkIO (forever (do { {-threadDelay 1000000 ;-} updatePreds predMap}))
                               res <- eval defs sys
                               killThread tid
                               putStrLn "Execution finished"
                               return ())
              putStrLn "Bad names in the specification"         --------


    parseSpec :: String -> IO ([ProcDef],[Claus])
    parseSpec file = do let f'= reverse(dropWhile isSpace (reverse file)) 
                        f <- readFile f'
                        (cspparser . lexer) f
