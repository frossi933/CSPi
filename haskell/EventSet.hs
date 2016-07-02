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

    insert v s = intersection (Set.singleton v) s

    intersection s q = if (Set.null s || Set.null q) then Set.empty
                                                     else Set.fold (\v r -> case Set.lookupIndex v q of
                                                                                Nothing -> r
                                                                                Just i -> Set.insert (mergeEvents v (Set.elemAt i q)) r)
                                                                   Set.empty
                                                                   s
                            where mergeEvents (E id1 v1 p1) (E id2 v2 p2) = E id1 (max v1 v2) (max p1 p2)
                                  mergeEvents v t = v
                                  max Nothing v = v
                                  max (Just v) _ = Just v

    union s q = Set.fold (\v s -> insert v s) q s

    difference :: EvSet -> EvSet -> EvSet
    difference = Set.difference
                        

    
