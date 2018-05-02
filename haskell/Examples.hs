module Examples where

    import Common
    import Csp
    import System.Random


    turn_on :: IO ()
    turn_on = putStrLn "On"

    turn_off :: IO ()
    turn_off = putStrLn "Off"

    pressed :: IO Bool
    pressed = do c <- (randomRIO (0, 1) :: IO Int)
                 return (c > 0)

    epsilon :: Event
    epsilon = Eps

    on :: Event
    on = newAction "on" turn_on

    press :: Event
    press = newCondition "press" pressed

    off :: Event
    off = newAction "off" turn_off

    dk_lampara :: Proc
    dk_lampara = on --> ( off --> (Ref dk_lampara) )

    dk_boton :: Proc
    dk_boton = press --> (Ref dk_boton)

    lamp :: Proc
    lamp = press --> ( on --> ( press --> ( off --> (Ref lamp) )))

    dks :: Proc
    dks = dk_lampara ||| dk_boton

    sistema :: Proc
    sistema = lamp ||| dks