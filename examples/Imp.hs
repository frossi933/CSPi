module Imp where

    press_pred :: IO Bool
    press_pred = do c <- getChar
                    return (c == 'p')

    char :: Char
    char = ''

    readChar :: IO ()
    readChar = do char <- getChar
                  return ()

    press1_pred :: IO Bool
    press1_pred = return (char == '1')

    press2_pred :: IO Bool
    press2_pred = return (char == '2')

    on_action :: IO ()
    on_action = putStrLn "ENCENDIDA"

    off_action :: IO ()
    off_action = putStrLn "APAGADA"


