module Imp where

    pressf :: IO Bool
    pressf = return True

    pressf2 :: IO Bool
    pressf2 = do --print "waiting char..."
                c <- getChar
                return (c == 'p')

    onf :: IO ()
    onf = do putStrLn "ENCENDIDA"
             return ()

    offf :: IO ()
    offf = do putStrLn "APAGADA"
