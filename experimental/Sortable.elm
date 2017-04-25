module Sortable exposing (Model, Msg, ItemDetails, ViewConfig, ListConfig, init, group, groupList, list, view)

{-| Sortable module provides the tools to easily create a sortable list.

# State
@docs Model, Msg, init

# View Configuration
@docs ItemDetails, ViewConfig, ListConfig, group, groupList, list

# View
@docs view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Model
import Geometry as Geo
import Utils exposing (..)
import Json.Decode as Json
import Dict exposing (Dict)


-- Model


{-| -}
type Model
    = Model
        { groups : Dict String (List Model.Item)
        , drag : Maybe Drag
        }


type alias Drag =
    { itemID : String
    , fromListID : String
    , hasDragged : Bool
    , pointerPos : Geo.Point
    }


{-| -}
init : Model
init =
    Model
        { groups = Dict.empty
        , drag = Nothing
        }



-- Update


{-| -}
type Msg
    = PointerDown String (List Model.Item) (Maybe String) Geo.Point



-- View


{-| ItemDetails represent a set of attributes and child nodes
-}
type alias ItemDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| ViewConfig is the configuration details of a list group
-}
type ViewConfig item msg
    = ViewConfig
        { id : String
        , toMsg : Msg -> msg
        , toID : item -> String
        , lists : List (ListConfig item msg)
        }


{-| ListConfig is the configuration details of a list
-}
type ListConfig item msg
    = ListConfig
        { id : String
        , tag : String
        , attributes : List (Attribute msg)
        , itemTag : String
        , itemDetails : item -> ItemDetails msg
        , handle : Maybe String
        , items : List item
        }


{-| -}
group :
    { id : String
    , toMsg : Msg -> msg
    , toID : item -> String
    , lists : List (ListConfig item msg)
    }
    -> ViewConfig item msg
group c =
    ViewConfig c


{-| -}
groupList :
    { id : String
    , tag : String
    , attributes : List (Attribute msg)
    , itemTag : String
    , itemDetails : item -> ItemDetails msg
    , handle : Maybe String
    , items : List item
    }
    -> ListConfig item msg
groupList c =
    ListConfig c


{-| -}
list :
    { id : String
    , tag : String
    , attributes : List (Attribute msg)
    , itemTag : String
    , itemDetails : item -> ItemDetails msg
    , handle : Maybe String
    , items : List item
    , toMsg : Msg -> msg
    , toID : item -> String
    }
    -> ViewConfig item msg
list { id, tag, attributes, itemTag, itemDetails, handle, items, toMsg, toID } =
    group
        { id = id
        , toMsg = toMsg
        , toID = toID
        , lists =
            [ groupList
                { id = id
                , tag = tag
                , attributes = attributes
                , itemTag = itemTag
                , itemDetails = itemDetails
                , handle = handle
                , items = items
                }
            ]
        }


sortViewConfig : Dict String (List Model.Item) -> ViewConfig item msg -> ViewConfig item msg
sortViewConfig sort (ViewConfig viewConfig) =
    let
        sorted =
            viewConfig.lists
                |> List.foldl (\list dict -> Dict.insert list.id list.items dict) (Dict.empty)
                |> Model.sortGroup viewConfig.toID sort
    in
        ViewConfig
            { viewConfig | lists = List.map (\list -> { list | items = Maybe.withDefault [] (Dict.get list.id sorted) }) viewConfig.lists }


viewConfigToItems : ViewConfig item msg -> List Model.Item
viewConfigToItems (ViewConfig conf) =
    conf.lists
        |> List.foldr
            (\list items ->
                items ++ List.map (\item -> Model.Item (conf.toID item) list.id) list.items
            )
            []


{-| -}
view : ViewConfig item msg -> String -> Model -> Html msg
view vc listID model =
    let
        (ViewConfig viewConfig) =
            sortViewConfig model.groups vc
    in
        viewConfig.lists
            |> find (\(ListConfig listConf) -> listConf.id == listID)
            |> Maybe.map (\listConf -> listView (ViewConfig viewConfig) listConf model)
            |> Maybe.withDefault (text "")


listView : ViewConfig item msg -> ListConfig item msg -> Model -> Html msg
listView (ViewConfig viewConf) (ListConfig listConf) (Model model) =
    let
        onMouseDown =
            Html.Attributes.map viewConf.toMsg <|
                onWithOptions "mousedown"
                    { defaultOptions | preventDefault = True }
                    (Json.map (PointerDown viewConf.id (viewConfigToItems (ViewConfig viewConf)) listConf.handle) clientPosition)

        attributes =
            listConf.attributes
                ++ case model.drag of
                    Just drag ->
                        []

                    Nothing ->
                        [ onMouseDown ]
    in
        Html.Keyed.node listConf.tag [] []
