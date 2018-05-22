module Csp where

    import qualified EventSet as EvSet
    import Common
    import Control.Monad
    import System.Random


    alphabet :: Proc -> EvSet.EvSet
    alphabet Stop = EvSet.empty
    alphabet Skip = EvSet.empty
    alphabet (Ref p) = EvSet.empty
    alphabet (Prefix v p) = EvSet.insert v $ alphabet p
    alphabet (ExtSel p q) = EvSet.union (alphabet p) (alphabet q)
    alphabet (IntSel p q) = EvSet.union (alphabet p) (alphabet q)
    alphabet (Seq p q) = EvSet.union (alphabet p) (alphabet q)
    alphabet (Inter p q) = EvSet.union (alphabet p) (alphabet q)
    alphabet (Parallel p q) = EvSet.union (alphabet p) (alphabet q)

    menu :: Proc -> EvSet.EvSet
    menu Stop = EvSet.empty
    menu Skip = EvSet.empty
    menu (Ref p) = EvSet.singleton Eps
    menu (Prefix v _) = EvSet.singleton v
    menu (ExtSel Skip p) = EvSet.insert Eps $ menu p
    menu (ExtSel p Skip) = EvSet.insert Eps $ menu p
    menu (ExtSel Stop Stop) = EvSet.singleton Eps
    menu (ExtSel p q) = EvSet.union (menu p) (menu q)
    menu (IntSel p q) = EvSet.singleton Eps
    menu (Seq Skip q) = EvSet.singleton Eps
    menu (Seq Stop q) = EvSet.singleton Eps
    menu (Seq p q) = menu p
    menu (Inter p q) = EvSet.union (menu p) (menu q)
    menu (Parallel Skip Skip) = EvSet.singleton Eps
    menu (Parallel p q) = EvSet.union (EvSet.intersection (menu p) (menu q))
                                      (EvSet.union (EvSet.difference (menu p) (alphabet q)) (EvSet.difference (menu q) (alphabet p)))

    menuIO :: Proc -> IO EvSet.EvSet
    menuIO Stop = return EvSet.empty
    menuIO Skip = return EvSet.empty
    menuIO (Ref p) = return $ EvSet.singleton Eps
    menuIO (Prefix v@(E _ Nothing _) _) = return $ EvSet.singleton v
    menuIO (Prefix v@(E id' (Just cond) _) _) = do b <- cond
                                                   if b then (return $ EvSet.singleton v) else return EvSet.empty
    menuIO (ExtSel Skip p) = do mp <- menuIO p
                                return $ EvSet.insert Eps mp
    menuIO (ExtSel p Skip) = do mp <- menuIO p
                                return $ EvSet.insert Eps mp
    menuIO (ExtSel Stop Stop) = return $ EvSet.singleton Eps
    menuIO (ExtSel p q) = do mp <- menuIO p
                             mq <- menuIO q
                             return $ EvSet.union mp mq
    menuIO (IntSel p q) = return $ EvSet.singleton Eps
    menuIO (Seq Skip q) = return $ EvSet.singleton Eps
    menuIO (Seq Stop q) = return $ EvSet.singleton Eps
    menuIO (Seq p q) = menuIO p
    menuIO (Inter p q) = do mp <- menuIO p
                            mq <- menuIO q
                            return $ EvSet.union mp mq
    menuIO (Parallel Skip Skip) = return $ EvSet.singleton Eps
    menuIO (Parallel p q) = do mp <- menuIO p
                               mq <- menuIO q
                               return (EvSet.union (EvSet.intersection mp mq)
                                                   (EvSet.union (EvSet.difference mp (alphabet q)) (EvSet.difference mq (alphabet p))))


    smallstep_eval :: Proc -> Event -> Maybe Proc
    smallstep_eval Stop _ = Nothing
    smallstep_eval Skip _ = Nothing
    smallstep_eval (Ref p) Eps = Just p
    smallstep_eval (Ref p) _ = Nothing
    smallstep_eval (Prefix e p) v = if (e == v) then Just p
                                                else Nothing
    smallstep_eval (ExtSel Stop Stop) Eps = Just Stop
    smallstep_eval (ExtSel Skip p) Eps = Just p
    smallstep_eval (ExtSel p Skip) Eps = Just p
    smallstep_eval (ExtSel p q) Eps = case (smallstep_eval p Eps, smallstep_eval q Eps) of
                                        (Nothing, Nothing) -> Nothing
                                        (Just p1, Nothing) -> Just (ExtSel p1 q)
                                        (Nothing, Just q1) -> Just (ExtSel p q1)
                                        (Just p1, Just q1) -> Just (ExtSel p1 q) -- TODO: random selection
    smallstep_eval (ExtSel p q) v = case (smallstep_eval p v, smallstep_eval q v) of
                                        (Nothing, Nothing) -> Nothing
                                        (Just p1, Nothing) -> Just p1
                                        (Nothing, Just q1) -> Just q1
                                        (Just p1, Just q1) -> Just p1 -- TODO: random selection
    smallstep_eval (IntSel p q) Eps = Just p -- TODO: random selection
    smallstep_eval (IntSel p q) _   = Nothing
    smallstep_eval (Seq Skip p) Eps = Just p
    smallstep_eval (Seq Stop _) Eps = Just Stop
    smallstep_eval (Seq p q) v = case smallstep_eval p v of
                                    Just p1 -> Just (Seq p1 q)
                                    Nothing -> Nothing
    smallstep_eval (Parallel Skip Skip) Eps = Just Skip
    smallstep_eval (Parallel p q) Eps = case (smallstep_eval p Eps, smallstep_eval q Eps) of
                                                (Just p1, Nothing) -> Just (Parallel p1 q)
                                                (Nothing, Just q1) -> Just (Parallel p q1)
                                                (Just p1, Just q1) -> Just (Parallel p1 q) -- TODO: random selection
                                                _                  -> Nothing
    smallstep_eval (Parallel p q) v = case (smallstep_eval p v, smallstep_eval q v) of
                                            (Just p1, Just q1) -> Just (Parallel p1 q1)
                                            (Just p1, Nothing) -> Just (Parallel p1 q)
                                            (Nothing, Just q1) -> Just (Parallel p q1)
                                            _                  -> Nothing
    smallstep_eval (Inter p q) v = case smallstep_eval q v of
                                        Just q1 -> Just q1
                                        Nothing -> case smallstep_eval p v of
                                                    Just p1 -> Just (Inter p1 q)
                                                    Nothing -> Nothing -- ??

    choose :: EvSet.EvSet -> IO Event
    choose s = do when debug $ putStrLn (show s)
                  if EvSet.member Eps s then return Eps else (do r <- randomRIO (0, (EvSet.size s)-1)       -- Eps has higher priority than concrete events
                                                                 return (EvSet.elemAt s r))


    eval :: Proc -> IO Proc
    eval Stop = do putStrLn "END: Stop"
                   return Stop
    eval Skip = do putStrLn "END: Success"
                   return Skip
    eval p = do m <- menuIO p
                --let m = menu p
                when debug $ print m
                if EvSet.isEmpty m then eval p                   -- waits for available events
                                   else do v <- choose m
                                           when debug $ putStrLn ("Menu: "++ show m ++ " Evento: " ++ show v ++ " -- Proceso: " ++ show p)
                                           case v of
                                             E _ Nothing Nothing -> case smallstep_eval p v of
                                                                       Nothing -> do putStrLn "END: Internal error"
                                                                                     return Stop
                                                                       Just p1 -> eval p1
                                             E id (Just cond) Nothing -> case smallstep_eval p v of
                                                                Nothing -> do putStrLn "END: Internal error"
                                                                              return Stop
                                                                Just p1 -> --do swapMVar bvar initial_state_mvar -- It could lose changes in bvar between its check in menu and this point
                                                                              eval p1
                                             E id Nothing (Just act) -> case smallstep_eval p v of
                                                                        Nothing -> do putStrLn "END: Internal error"
                                                                                      return Stop
                                                                        Just p1 -> do res <- act
                                                                                      eval p1
                                             E id (Just bvar) (Just act) -> case smallstep_eval p v of
                                                                            Nothing -> do putStrLn "END: Internal error"
                                                                                          return Stop
                                                                            Just p1 -> do act
                                                                                          eval p1
                                             _ -> case smallstep_eval p v of
                                                    Nothing -> do putStrLn "END: Internal error"
                                                                  return Stop
                                                    Just p1 -> eval p1

    deadlock :: Proc -> Bool
    deadlock Skip = False
    deadlock Stop = False -- FIXME
    deadlock (Ref p) = False -- FIXME
    deadlock (Prefix e p) = deadlock p
    deadlock (ExtSel p q) = deadlock p || deadlock q
    deadlock (IntSel p q) = deadlock p || deadlock q
    deadlock (Seq p q) = deadlock p || deadlock q
    deadlock (Parallel p q) = (not (EvSet.isEmpty (menu p))
                               && not (EvSet.isEmpty (menu q))
                               && EvSet.isEmpty (EvSet.intersection (menu p) (menu q))
                               && EvSet.subset (menu p) (alphabet q)
                               && EvSet.subset (menu q) (alphabet p))
                              || deadlock p
                              || deadlock q
    deadlock (Inter p q) = deadlock p || deadlock q  -- FIXME
