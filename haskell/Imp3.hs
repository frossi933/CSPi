-- no importa el nombre del archivo, pero el modulo debe llamarse Imp
module Imp where

    pressf :: IO Bool
    pressf = return True

    pressf2 :: IO Bool
    pressf2 = do --print "waiting char..."
                c <- getChar
                return (c == 'p')

    press12f :: IO Bool
    press12f = do c <- getChar
                  

    onf :: IO ()
    onf = do putStrLn "ENCENDIDA"
             return ()

    offf :: IO ()
    offf = do putStrLn "APAGADA"
