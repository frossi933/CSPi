module EventSet where

    import qualified Data.List as List
    import Common

    type EvSet = [Event]

    empty::EvSet
    empty = []

    isEmpty :: EvSet -> Bool
    isEmpty = List.null

    member :: Event -> EvSet -> Bool
    member = List.elem

    size :: EvSet -> Int
    size = List.length

    elemAt :: EvSet -> Int -> Event
    elemAt = (!!)

    singleton :: Event -> EvSet
    singleton = (:[])

    insert :: Event -> EvSet -> EvSet
    insert v s = case List.elemIndex v s of
                    Nothing -> (v:s)
                    Just i -> let t = (s!!i) in (mergeEvents v t) : (List.delete t s)

    intersection :: EvSet -> EvSet -> EvSet
    --intersection = List.intersect
    intersection s q = if (isEmpty s || isEmpty q) then empty
                                             else List.foldr (\v r -> case List.elemIndex v q of
                                                                            Nothing -> r
                                                                            Just i -> insert (mergeEvents v (q!!i)) r)
                                                             empty
                                                             s

    mergeEvents (E id1 v1 p1) (E id2 v2 p2) = E id1 (maxMaybe v1 v2) (maxMaybe p1 p2)
    mergeEvents v t = v

    maxMaybe Nothing v = v
    maxMaybe (Just v) _ = Just v

    union :: EvSet -> EvSet -> EvSet
    union s q = List.foldr (\v s -> insert v s) q s

    difference :: EvSet -> EvSet -> EvSet
    difference = (List.\\)
