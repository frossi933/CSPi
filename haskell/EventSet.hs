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

    insert v@(In _ _) s = if Set.member v s then s else Set.insert v s
    insert v s = Set.insert v s -- This replaces the old one if it exists

    intersection s q = if (Set.null s || Set.null q) then Set.empty
                                                     else let s' = Set.filter (filtOut q) s
                                                          in Set.fold (\v q' -> if Set.member v s then insert v q' else q') s' q
                            where filtOut r v@(Out _ _) = Set.member v r
                                  filtOut _ _ = False

    union s q = Set.fold (\v s -> insert v s) q s

    difference :: EvSet -> EvSet -> EvSet
    difference = Set.difference
                        

    
