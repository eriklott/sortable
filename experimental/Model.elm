module Model exposing (..)

import Dict exposing (Dict)
import Set exposing (Set)


type alias Item =
    { id : String
    , listID : String
    }


{-| merges the positions and listIDs from sortGroup items onto to intersecting
items in the sourceGroup, and returns the sourceGroup.
-}
sortGroup : (item -> String) -> List Item -> Dict String (List item) -> Dict String (List item)
sortGroup toID sort source =
    let
        sourceListIDS =
            source
                |> Dict.keys
                |> Set.fromList

        sourceDict =
            source
                |> Dict.foldl
                    (\listID items dest ->
                        items
                            |> List.foldl
                                (\item ( dest, idx ) ->
                                    ( Dict.insert (toID item)
                                        { id = (toID item)
                                        , listID = listID
                                        , idx = idx + 10000000
                                        , item = item
                                        }
                                        dest
                                    , idx + 1
                                    )
                                )
                                ( dest, 0 )
                            |> Tuple.first
                    )
                    (Dict.empty)

        mergedDict =
            sort
                |> List.foldl
                    (\item ( dict, idx ) ->
                        if Dict.member item.id dict && Set.member item.listID sourceListIDS then
                            ( Dict.update item.id
                                (Maybe.map (\rec -> { rec | listID = item.listID, idx = idx }))
                                dict
                            , idx + 1
                            )
                        else
                            ( dict, idx )
                    )
                    ( sourceDict, 0 )
                |> Tuple.first

        sortedDict =
            mergedDict
                |> Dict.values
                |> List.sortBy .idx
                |> List.foldr
                    (\item dict ->
                        if Dict.member item.listID dict then
                            Dict.insert item.listID [ item.item ] dict
                        else
                            Dict.update item.listID (Maybe.map (\items -> item.item :: items)) dict
                    )
                    (Dict.empty)
    in
        sortedDict
