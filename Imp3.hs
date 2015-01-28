module Imp3 where

    boton_ab :: IO Bool
    boton_ab = return False

    hay_item :: IO Bool
    hay_item = return True

    prensar :: IO ()
    prensar = putStrLn "Prensando..."
    
    item :: IO ()
    item = putStrLn "Nuevo Item..."