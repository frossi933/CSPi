module Imp where

    pressf2 :: IO Bool
    pressf2 = return True

    pressf :: IO Bool
    pressf = do --print "waiting char..."
                c <- getChar
                return (c == 'p')

    onf :: IO ()
    onf = do putStrLn "ENCENDIDA"
             return ()

    offf :: IO ()
    offf = do putStrLn "APAGADA"
