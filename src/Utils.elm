module Utils exposing (..)

{-| Finds and returns the first item in the list satisfying the predicate.
-}


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first
            else
                find predicate rest


{-| Finds and returns the first item, and the first items index, in the list
satisfying the predicate.
-}
indexedFind : (a -> Bool) -> List a -> Maybe ( Int, a )
indexedFind predicate list =
    let
        loop index list =
            case list of
                [] ->
                    Nothing

                first :: rest ->
                    if predicate first then
                        Just ( index, first )
                    else
                        loop (index + 1) rest
    in
        loop 0 list
