module Sortable exposing (Model, Msg, ViewDetails, GroupConfig, GroupListConfig, init, update, subscriptions, group, groupList, view)

{-| Sortable module provides the tools to easily create a sortable list.

# State
@docs Model, Msg, init, update, subscriptions

# View Configuration
@docs ViewDetails, GroupConfig, GroupListConfig, group, groupList, view

# View
@docs view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as Json
import Dict exposing (Dict)
import Set
import BoundingBox
import Math.Vector2 as Vec2


-- Model


{-| Model represents the state of the sortable list
-}
type Model
    = Model
        { items : Dict String ( String, Int )
        , draggingItem : Maybe DraggingItem
        }


{-| DraggingItem contains state relating to the item currently being dragged
-}
type alias DraggingItem =
    { id : String
    , bounds : Rect
    , fromListID : String
    , hasDragged : Bool
    , position : Point
    }


{-| returns the initial state of the sortable list
-}
init : Model
init =
    Model
        { items = Dict.empty
        , draggingItem = Nothing
        }



-- Geometry


type alias Rect =
    BoundingBox.BoundingBox


type alias Point =
    Vec2.Vec2


{-| Returns true if p2 intersects with the half of the bounds which is opposite
to the general direction of the movement from p1 to p2. For example, if the
direction of p1-p2 is "Right", the function will check for the intersection of
p2 and the right half of the bounds.
-}
detectSideIntersect : Point -> Point -> Rect -> Bool
detectSideIntersect p1 p2 b =
    let
        contains bnds =
            BoundingBox.contains p2 bnds
    in
        case getDirection p1 p2 of
            Up ->
                contains <| getRectHalf TopSide b

            Down ->
                contains <| getRectHalf BottomSide b

            Left ->
                contains <| getRectHalf LeftSide b

            Right ->
                contains <| getRectHalf RightSide b

            NoDirection ->
                False


type Direction
    = Up
    | Down
    | Left
    | Right
    | NoDirection


{-| Returns the general direction of the movement from p1 to p2.
-}
getDirection : Point -> Point -> Direction
getDirection p1 p2 =
    let
        delta =
            Vec2.sub p2 p1

        x =
            Vec2.getX delta

        y =
            Vec2.getY delta
    in
        Debug.log "direction" <|
            if x == 0 && y == 0 then
                NoDirection
            else if abs x > abs y then
                if x > 0 then
                    Right
                else
                    Left
            else if y > 0 then
                Down
            else
                Up


type Side
    = TopSide
    | BottomSide
    | LeftSide
    | RightSide


{-| Returns one half of the bounds provided.
-}
getRectHalf : Side -> Rect -> Rect
getRectHalf side bounds =
    let
        center =
            BoundingBox.center bounds

        topLeft =
            BoundingBox.bottomLeft bounds

        bottomRight =
            BoundingBox.topRight bounds
    in
        case side of
            TopSide ->
                BoundingBox.fromCorners
                    (Vec2.fromTuple ( Vec2.getX topLeft, Vec2.getY topLeft ))
                    (Vec2.fromTuple ( Vec2.getX bottomRight, Vec2.getY center ))

            BottomSide ->
                BoundingBox.fromCorners
                    (Vec2.fromTuple ( Vec2.getX topLeft, Vec2.getY center ))
                    (Vec2.fromTuple ( Vec2.getX bottomRight, Vec2.getY bottomRight ))

            LeftSide ->
                BoundingBox.fromCorners
                    (Vec2.fromTuple ( Vec2.getX topLeft, Vec2.getY topLeft ))
                    (Vec2.fromTuple ( Vec2.getX center, Vec2.getY bottomRight ))

            RightSide ->
                BoundingBox.fromCorners
                    (Vec2.fromTuple ( Vec2.getX center, Vec2.getY topLeft ))
                    (Vec2.fromTuple ( Vec2.getX bottomRight, Vec2.getY bottomRight ))



-- Update


{-| Sortable list message
-}
type Msg
    = PointerDown
    | PointerEmptyListIntersect String


{-| updates the sortable list model
-}
update : Msg -> Model -> Model
update msg model =
    model



-- Subscriptions


{-| subscribes to various global events
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- View Config


{-| ViewDetails represent a set of attributes and child nodes
-}
type alias ViewDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| GroupConfig is the configuration details of a list group
-}
type GroupConfig item msg
    = GroupConfig
        { toMsg : Msg -> msg
        , toID : item -> String
        , lists : List (GroupListConfig item msg)
        , model : Model
        }


{-| GroupListConfig is the configuration details of a list
-}
type GroupListConfig item msg
    = GroupListConfig
        { id : String
        , tag : String
        , attributes : List (Attribute msg)
        , itemTag : String
        , itemDetails : item -> ViewDetails msg
        , handle : Maybe String
        , items : List item
        }


type alias ListConfig item msg =
    { id : String
    , tag : String
    , attributes : List (Attribute msg)
    , itemTag : String
    , itemDetails : item -> ViewDetails msg
    , itemHandle : Maybe String
    , toMsg : Msg -> msg
    , toID : item -> String
    }


groupConfigToListConfig : GroupConfig item msg -> GroupListConfig item msg -> ListConfig item msg
groupConfigToListConfig (GroupConfig groupConfig) (GroupListConfig groupListConfig) =
    { id = groupListConfig.id
    , tag = groupListConfig.tag
    , attributes = groupListConfig.attributes
    , itemTag = groupListConfig.itemTag
    , itemDetails = groupListConfig.itemDetails
    , itemHandle = groupListConfig.handle
    , toMsg = groupConfig.toMsg
    , toID = groupConfig.toID
    }


sortGroupConfig : GroupConfig item msg -> GroupConfig item msg
sortGroupConfig (GroupConfig config) =
    let
        (Model model) =
            config.model

        listIDS =
            config.lists
                |> List.map (\(GroupListConfig list) -> list.id)
                |> Set.fromList

        sortedLists : Dict String (List item)
        sortedLists =
            config.lists
                |> List.foldr
                    (\(GroupListConfig list) ( intersection, rest, index ) ->
                        list.items
                            |> List.foldr
                                (\item ( intersection, rest, index ) ->
                                    let
                                        getCachedItem =
                                            Dict.get (config.toID item) model.items

                                        checkListIDExists ( cachedListID, cachedPosition ) =
                                            if Set.member cachedListID listIDS then
                                                Just ( cachedListID, cachedPosition )
                                            else
                                                Nothing

                                        addItemToIntersection ( cachedListID, cachedPosition ) =
                                            Just ( { item = item, listID = cachedListID, position = cachedPosition } :: intersection, rest, index )

                                        addItemToRest =
                                            ( intersection, { item = item, listID = list.id, position = index } :: rest, index - 1 )
                                    in
                                        getCachedItem
                                            |> Maybe.andThen checkListIDExists
                                            |> Maybe.andThen addItemToIntersection
                                            |> Maybe.withDefault addItemToRest
                                )
                                ( intersection, rest, index )
                    )
                    ( [], [], 0 )
                |> (\( intersection, rest, idx ) ->
                        (List.sortBy .position intersection) ++ intersection
                   )
                |> List.foldr
                    (\{ item, listID, position } dict ->
                        if Dict.member listID dict then
                            Dict.update listID (Maybe.map (\items -> item :: items)) dict
                        else
                            Dict.insert listID [ item ] dict
                    )
                    (Dict.empty)

        updatedLists =
            config.lists
                |> List.map
                    (\(GroupListConfig list) ->
                        GroupListConfig { list | items = (Dict.get list.id sortedLists) |> Maybe.withDefault list.items }
                    )
    in
        GroupConfig { config | lists = updatedLists }


{-| -}
group :
    { toMsg : Msg -> msg
    , toID : item -> String
    , lists : List (GroupListConfig item msg)
    }
    -> Model
    -> GroupConfig item msg
group { toMsg, toID, lists } (Model model) =
    sortGroupConfig <|
        GroupConfig
            { toMsg = toMsg
            , toID = toID
            , lists = lists
            , model = Model model
            }


{-| -}
groupList :
    { id : String
    , tag : String
    , attributes : List (Attribute msg)
    , itemTag : String
    , itemDetails : item -> ViewDetails msg
    , handle : Maybe String
    , items : List item
    }
    -> GroupListConfig item msg
groupList c =
    GroupListConfig c



-- View


{-| -}
view : GroupConfig item msg -> String -> Html msg
view (GroupConfig groupConfig) listID =
    let
        groupListConfigByListID (GroupListConfig groupListConfig) =
            groupListConfig.id == listID

        renderListView (GroupListConfig groupListConfig) =
            listView
                (groupConfigToListConfig
                    (GroupConfig groupConfig)
                    (GroupListConfig groupListConfig)
                )
                groupConfig.model
                groupListConfig.items

        renderNothing =
            text ""
    in
        groupConfig.lists
            |> find groupListConfigByListID
            |> Maybe.map renderListView
            |> Maybe.withDefault renderNothing



-- List Views


listView : ListConfig item msg -> Model -> List item -> Html msg
listView config (Model model) items =
    case model.draggingItem of
        Just draggingItem ->
            if draggingItem.hasDragged then
                if List.isEmpty items then
                    emptyListView config (Model model)
                else
                    activeListView config (Model model) items draggingItem
            else
                idleListView config items

        Nothing ->
            idleListView config items


emptyListView : ListConfig item msg -> Model -> Html msg
emptyListView config (Model model) =
    let
        localAttributes =
            case model.draggingItem of
                Just _ ->
                    [ on "mouseover" <| Json.succeed <| config.toMsg <| PointerEmptyListIntersect config.id ]

                Nothing ->
                    []

        attributes =
            config.attributes ++ localAttributes
    in
        Html.Keyed.node config.tag attributes []


idleListView : ListConfig item msg -> List item -> Html msg
idleListView config items =
    let
        children =
            List.map (idleItemView config) items
    in
        Html.Keyed.node config.tag config.attributes children


activeListView : ListConfig item msg -> Model -> List item -> DraggingItem -> Html msg
activeListView config (Model model) items draggingItem =
    let
        itemView item ( children, index ) =
            if config.toID item == draggingItem.id then
                ( draggingItemView config item :: children ++ [ cloneItemView config draggingItem item ]
                , index - 1
                )
            else
                ( targetItemView config index item :: children
                , index - 1
                )

        children =
            items
                |> List.foldr itemView ( [], (List.length items) - 1 )
                |> Tuple.first
    in
        Html.Keyed.node config.tag config.attributes children



-- Item Views


idleItemView : ListConfig item msg -> item -> ( String, Html msg )
idleItemView config item =
    let
        id =
            config.toID item

        itemDetails =
            config.itemDetails item

        onItemMouseDown tagger =
            onWithOptions "mousedown"
                { defaultOptions | preventDefault = True }
                (Json.map tagger clientXY)

        -- onHandleMouseDown handle tagger =
        --     onWithOptions "mousedown"
        --         { defaultOptions | preventDefault = True }
        --         (target className
        --             |> Json.andThen
        --                 (\classes ->
        --                     if classNamesMember handle classes then
        --                         Json.map tagger clientXY
        --                     else
        --                         Json.fail ("no mousedown on handle '." ++ handle ++ "'")
        --                 )
        --         )
        -- onMouseDown handle tagger =
        --     case handle of
        --         Just handle_ ->
        --             onHandleMouseDown handle_ tagger
        --
        --         Nothing ->
        --             onItemMouseDown tagger
        -- attributes =
        --     viewData.attributes
        --         ++ [ class (itemClass id)
        --            , Html.Attributes.map config.toMsg <| onMouseDown config.handle (PointerDown id)
        --            ]
        localAttributes =
            [ class (itemClass id) ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node config.itemTag attributes children )


draggingItemView : ListConfig item msg -> item -> ( String, Html msg )
draggingItemView config item =
    let
        id =
            config.toID item

        itemDetails =
            config.itemDetails item

        localAttributes =
            [ class (itemClass id)
            , style [ ( "opacity", "0.5" ) ]
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node config.itemTag attributes children )


targetItemView : ListConfig item msg -> Int -> item -> ( String, Html msg )
targetItemView config index item =
    let
        id =
            config.toID item

        itemDetails =
            config.itemDetails item

        localAttributes =
            [ class (itemClass id) ]

        -- ++ [ class (itemClass id)
        --    , on "mousemove" (Json.succeed (PointerItemIntersect config.id id idx))
        --         |> Html.Attributes.map config.toMsg
        --    ]
        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node config.itemTag attributes children )


cloneItemView : ListConfig item msg -> DraggingItem -> item -> ( String, Html msg )
cloneItemView config draggingItem item =
    let
        id =
            "clone-" ++ config.toID item

        itemDetails =
            config.itemDetails item

        bounds =
            BoundingBox.translate draggingItem.position draggingItem.bounds

        topLeft =
            BoundingBox.bottomLeft bounds

        top =
            Vec2.getY topLeft

        left =
            Vec2.getX topLeft

        width =
            BoundingBox.width bounds

        height =
            BoundingBox.height bounds

        localAttributes =
            [ style
                [ ( "position", "fixed" )
                , ( "top", px top )
                , ( "left", px left )
                , ( "width", px width )
                , ( "height", px height )
                , ( "z-index", "9999" )
                , ( "pointer-events", "none" )
                ]
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node config.itemTag attributes children )



-- Decoders


clientXY : Json.Decoder Position
clientXY =
    Json.map2 (,)
        (Json.field "clientX" Json.float)
        (Json.field "clientY" Json.float)
        |> Json.map Vec2.fromTuple


currentTarget : Json.Decoder msg -> Json.Decoder msg
currentTarget =
    Json.field "currentTarget"


target : Json.Decoder msg -> Json.Decoder msg
target =
    Json.field "target"


className : Json.Decoder String
className =
    Json.field "className" Json.string



-- CSS Helpers


px : v -> String
px v =
    toString v ++ "px"


itemClass : String -> String
itemClass itemID =
    "sortable-" ++ itemID


classSelector : String -> String
classSelector className =
    "." ++ className


classNamesMember : String -> String -> Bool
classNamesMember class classes =
    String.split " " classes
        |> List.member class



-- List Helpers


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
