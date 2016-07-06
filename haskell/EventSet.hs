module EventSet where

    import qualified Data.Set as Set
    import Common

    empty = Set.empty

    member :: Event -> EvSet -> Bool
    member = Set.member

    null = Set.null
    
    size = Set.size

    elemAt = Set.elemAt

    singleton = Set.singleton

    insert v s = case Set.lookupIndex v s of
                    Nothing -> Set.insert v s
                    Just i -> Set.insert (mergeEvents v (Set.elemAt i s)) s

    intersection s q = if (Set.null s || Set.null q) then Set.empty
                                                     else Set.fold (\v r -> case Set.lookupIndex v q of
                                                                                Nothing -> r
                                                                                Just i -> Set.insert (mergeEvents v (Set.elemAt i q)) r)
                                                                   Set.empty
                                                                   s
    
    mergeEvents (E id1 v1 p1) (E id2 v2 p2) = E id1 (maxMaybe v1 v2) (maxMaybe p1 p2)
    mergeEvents v t = v

    maxMaybe Nothing v = v
    maxMaybe (Just v) _ = Just v

    union s q = Set.fold (\v s -> insert v s) q s

    difference :: EvSet -> EvSet -> EvSet
    difference = Set.difference
                        

    
