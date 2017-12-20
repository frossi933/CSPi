module Imp where

    press_pred :: IO Bool
    press_pred = do c <- getChar
                    return (c == 'p')

    press1_pred :: IO Bool
    press1_pred = do c <- getChar
                     return (c == '1')

    press2_pred :: IO Bool
    press2_pred = do c <- getChar
                     return (c == '2')

    on_action :: IO ()
    on_action = putStrLn "ENCENDIDA"

    off_action :: IO ()
    off_action = putStrLn "APAGADA"
