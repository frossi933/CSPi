module Imp where

    pressf :: IO Bool
    pressf = do c <- getChar
                if c == 'p' then return True else return False

    onf :: IO ()
    onf = do putStrLn "ENCENDIDA"
             return ()

    offf :: IO ()
    offf = do putStrLn "APAGADA"
