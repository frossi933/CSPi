module Imp3 where

    boton_ab :: IO Bool
    boton_ab = return False

    hay_item :: IO Bool
    hay_item = return True

    prensar :: IO String
    prensar = do putStrLn "Prensando..."
                 return "Prensado"
    
    item :: IO ()
    item = putStrLn "Nuevo Item..."
