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
import DOM
import Mouse


-- Model


{-| Model represents the state of the sortable list
-}
type Model
    = Model
        { sortState : SortState
        , draggingItem : Maybe DraggingItem
        }


type alias SortState =
    Dict String ( String, Int )


{-| DraggingItem contains state relating to the item currently being dragged
-}
type alias DraggingItem =
    { id : String
    , bounds : Rect
    , fromListID : String
    , hasDragged : Bool
    , currentPos : Point
    , prevPos : Point
    }


{-| returns the initial state of the sortable list
-}
init : Model
init =
    Model
        { sortState = Dict.empty
        , draggingItem = Nothing
        }


moveItem : String -> String -> Int -> Model -> Model
moveItem itemID nextListID nextIndex (Model model) =
    let
        decrementCurrentSiblingPositions ( currentListID, currentIndex ) =
            Model
                { model
                    | sortState =
                        Dict.map
                            (\k ( listID, index ) ->
                                if listID == currentListID && index > currentIndex then
                                    ( listID, index - 1 )
                                else
                                    ( listID, index )
                            )
                            model.sortState
                }

        incrementNextSiblingPositions (Model model) =
            Model
                { model
                    | sortState =
                        Dict.map
                            (\k ( listID, index ) ->
                                if listID == nextListID && index >= nextIndex then
                                    ( listID, index + 1 )
                                else
                                    ( listID, index )
                            )
                            model.sortState
                }

        insertItem (Model model) =
            Model { model | sortState = Dict.insert itemID ( nextListID, nextIndex ) model.sortState }
    in
        Dict.get itemID model.sortState
            |> Maybe.map
                (decrementCurrentSiblingPositions
                    >> incrementNextSiblingPositions
                    >> insertItem
                )
            |> Maybe.withDefault (Model model)



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


getItemRectSync : String -> Maybe Rect
getItemRectSync itemID =
    let
        dimensionsToRect dimensions =
            BoundingBox.fromCorners
                (Vec2.fromTuple ( dimensions.left, dimensions.top ))
                (Vec2.fromTuple ( dimensions.right, dimensions.bottom ))
    in
        DOM.getDimensionsSync (classSelector <| itemClass itemID)
            |> Result.map dimensionsToRect
            |> Result.toMaybe



-- Update


{-| Sortable list message
-}
type Msg
    = PointerDown SortState String String Point
    | PointerMove Point
    | PointerOverEmptyList String
    | PointerOverTargetItem String String Int
    | PointerUp


{-| updates the sortable list model
-}
update : Msg -> Model -> Model
update msg (Model model) =
    case Debug.log "msg" msg of
        PointerDown sortState listID itemID position ->
            case getItemRectSync itemID of
                Just bounds ->
                    Model
                        { model
                            | sortState = sortState
                            , draggingItem =
                                Just
                                    { id = itemID
                                    , bounds = BoundingBox.translate (Vec2.negate position) bounds
                                    , fromListID = listID
                                    , hasDragged = False
                                    , currentPos = position
                                    , prevPos = position
                                    }
                        }

                Nothing ->
                    Model model

        PointerMove position ->
            case model.draggingItem of
                Just draggingItem ->
                    Model
                        { model
                            | draggingItem =
                                Just
                                    { draggingItem
                                        | currentPos = position
                                        , prevPos = draggingItem.currentPos
                                        , hasDragged = draggingItem.hasDragged || draggingItem.currentPos /= position
                                    }
                        }

                Nothing ->
                    Model model

        PointerOverEmptyList listID ->
            case model.draggingItem of
                Just draggingItem ->
                    moveItem draggingItem.id listID 0 (Model model)

                Nothing ->
                    Model model

        PointerOverTargetItem targetListID targetItemID targetIndex ->
            case model.draggingItem of
                Just draggingItem ->
                    case getItemRectSync targetItemID of
                        Just targetBounds ->
                            if detectSideIntersect draggingItem.prevPos draggingItem.currentPos targetBounds then
                                moveItem draggingItem.id targetListID targetIndex (Model model)
                            else
                                (Model model)

                        Nothing ->
                            (Model model)

                Nothing ->
                    Model model

        PointerUp ->
            Model { model | draggingItem = Nothing }



-- Subscriptions


{-| subscribes to various global events
-}
subscriptions : Model -> Sub Msg
subscriptions (Model model) =
    case model.draggingItem of
        Just _ ->
            Sub.batch
                [ Mouse.moves (\p -> PointerMove p.clientXY)
                , Mouse.ups (\_ -> PointerUp)
                ]

        Nothing ->
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
        , sortState : SortState
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


applySortStateToGroupConfig : GroupConfig item msg -> GroupConfig item msg
applySortStateToGroupConfig (GroupConfig config) =
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
                    (\(GroupListConfig list) ( intersection, rest ) ->
                        list.items
                            |> List.foldr
                                (\item ( intersection, rest ) ->
                                    let
                                        getCachedItem =
                                            Dict.get (config.toID item) model.sortState

                                        checkListIDExists ( listID, position ) =
                                            if Set.member listID listIDS then
                                                Just ( listID, position )
                                            else
                                                Nothing

                                        addItemToIntersection ( listID, position ) =
                                            Just ( { item = item, listID = listID, position = position } :: intersection, rest )

                                        addItemToRest =
                                            ( intersection, { item = item, listID = list.id, position = 0 } :: rest )
                                    in
                                        getCachedItem
                                            |> Maybe.andThen checkListIDExists
                                            |> Maybe.andThen addItemToIntersection
                                            |> Maybe.withDefault addItemToRest
                                )
                                ( intersection, rest )
                    )
                    ( [], [] )
                |> (\( intersection, rest ) ->
                        (List.sortBy .position intersection) ++ rest
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
                        GroupListConfig { list | items = (Dict.get list.id sortedLists) |> Maybe.withDefault [] }
                    )
    in
        GroupConfig { config | lists = updatedLists }


groupConfigToSortState : GroupConfig item msg -> SortState
groupConfigToSortState (GroupConfig config) =
    (config.lists
        |> List.foldl
            (\(GroupListConfig groupListConfig) sortState ->
                groupListConfig.items
                    |> List.foldl
                        (\item ( sortState, index ) ->
                            ( Dict.insert (config.toID item) ( groupListConfig.id, index ) sortState, index + 1 )
                        )
                        ( sortState, 0 )
                    |> Tuple.first
            )
            (Dict.empty)
    )


{-| -}
group :
    { toMsg : Msg -> msg
    , toID : item -> String
    , lists : List (GroupListConfig item msg)
    }
    -> Model
    -> GroupConfig item msg
group { toMsg, toID, lists } model =
    let
        (GroupConfig groupConfig) =
            GroupConfig
                { toMsg = toMsg
                , toID = toID
                , lists = lists
                , model = model
                , sortState = Dict.empty
                }

        (GroupConfig groupConfig_) =
            applySortStateToGroupConfig (GroupConfig groupConfig)
    in
        GroupConfig { groupConfig_ | sortState = groupConfigToSortState (GroupConfig groupConfig_) }


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
                groupConfig.sortState
                groupListConfig.items

        renderNothing =
            text ""
    in
        groupConfig.lists
            |> find groupListConfigByListID
            |> Maybe.map renderListView
            |> Maybe.withDefault renderNothing



-- List Views


listView : ListConfig item msg -> Model -> SortState -> List item -> Html msg
listView config (Model model) sortState items =
    case model.draggingItem of
        Just draggingItem ->
            if draggingItem.hasDragged then
                if List.isEmpty items then
                    emptyListView config (Model model)
                else
                    activeListView config (Model model) items draggingItem
            else
                idleListView config sortState items

        Nothing ->
            idleListView config sortState items


emptyListView : ListConfig item msg -> Model -> Html msg
emptyListView config (Model model) =
    let
        localAttributes =
            case model.draggingItem of
                Just _ ->
                    [ on "mouseover" <| Json.succeed <| config.toMsg <| PointerOverEmptyList config.id ]

                Nothing ->
                    []

        attributes =
            config.attributes ++ localAttributes
    in
        Html.Keyed.node config.tag attributes []


idleListView : ListConfig item msg -> SortState -> List item -> Html msg
idleListView config cachedItems items =
    let
        children =
            List.map (idleItemView config cachedItems) items
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


idleItemView : ListConfig item msg -> SortState -> item -> ( String, Html msg )
idleItemView config cachedItems item =
    let
        id =
            config.toID item

        itemDetails =
            config.itemDetails item

        onItemMouseDown tagger =
            onWithOptions "mousedown"
                { defaultOptions | preventDefault = True }
                (Json.map tagger clientXY)

        onHandleMouseDown handle tagger =
            onWithOptions "mousedown"
                { defaultOptions | preventDefault = True }
                (target className
                    |> Json.andThen
                        (\classes ->
                            if classNamesMember handle classes then
                                Json.map tagger clientXY
                            else
                                Json.fail ("no mousedown on handle '." ++ handle ++ "'")
                        )
                )

        onMouseDown tagger =
            case config.itemHandle of
                Just handle ->
                    onHandleMouseDown handle tagger

                Nothing ->
                    onItemMouseDown tagger

        localAttributes =
            [ class (itemClass id)
            , Html.Attributes.map config.toMsg <| onMouseDown <| PointerDown cachedItems config.id id
            ]

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
            [ class (itemClass id)
            , on "mousemove" (Json.succeed (PointerOverTargetItem config.id id index))
                |> Html.Attributes.map config.toMsg
            ]

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
            BoundingBox.translate draggingItem.currentPos draggingItem.bounds

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
                , ( "list-style", "none" )
                ]
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node config.itemTag attributes children )



-- Decoders


clientXY : Json.Decoder Point
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
