module Sortable exposing (State, Msg, ViewDetails, Config, Sort, init, update, subscriptions, config, view)

{-| Sortable module provides the tools to easily create a sortable list.

# State
@docs State, init, Msg, update, subscriptions

# Configuration
@docs Config, config, ViewDetails

# View
@docs view

# Events
@docs Sort


-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed exposing (..)
import Mouse
import Json.Decode as Json
import BoundingBox
import Math.Vector2 as Vec2
import DOM


-- State


{-| The state of sorting.
-}
type State
    = Idle
    | Dragging DraggingItem


{-| DraggingItem represents the model of the current drag.

bounds:
the bounds of the dragging item dom node, translated such that the
dragStart position is locationed a coord 0,0

prevPos:
previous position of the pointer relative to the viewport

currentPos:
current position of the pointer relative to the viewport
-}
type alias DraggingItem =
    { id : String
    , bounds : Rect
    , hasDragged : Bool
    , prevPos : Position
    , currentPos : Position
    }


type alias Rect =
    BoundingBox.BoundingBox


type alias Position =
    Vec2.Vec2


{-| The initial state of a sortable list
-}
init : State
init =
    Idle



-- Events


{-| Sort event is returned when an item has changed location
-}
type alias Sort =
    { listID : String
    , itemID : String
    , index : Int
    }



-- Update


{-| Msg represents a sortable list message
-}
type Msg
    = PointerDown String Position
    | PointerMove Position
    | PointerItemIntersect String String Int
    | PointerEmptyListIntersect String
    | PointerUp


{-| update returns a new state
-}
update : (Sort -> msg) -> msg -> Msg -> State -> ( State, Maybe msg )
update toSort toEnd msg state =
    case ( state, msg ) of
        ( Idle, PointerDown itemID position ) ->
            case getItemRectSync itemID of
                Just bounds ->
                    ( Dragging
                        { id = itemID
                        , bounds = BoundingBox.translate (Vec2.negate position) bounds
                        , prevPos = position
                        , currentPos = position
                        , hasDragged = False
                        }
                    , Nothing
                    )

                Nothing ->
                    ( state, Nothing )

        ( Dragging draggingItem, PointerMove position ) ->
            ( Dragging
                { draggingItem
                    | prevPos = draggingItem.currentPos
                    , currentPos = position
                    , hasDragged = draggingItem.hasDragged || draggingItem.currentPos /= position
                }
            , Nothing
            )

        ( Dragging draggingItem, PointerItemIntersect listID itemID index ) ->
            case getItemRectSync itemID of
                Just bounds ->
                    if detectSideIntersect draggingItem.prevPos draggingItem.currentPos bounds then
                        ( Dragging draggingItem
                        , Just <|
                            toSort
                                { listID = listID
                                , itemID = draggingItem.id
                                , index = index
                                }
                        )
                    else
                        ( Dragging draggingItem, Nothing )

                Nothing ->
                    ( Dragging draggingItem, Nothing )

        ( Dragging draggingItem, PointerEmptyListIntersect listID ) ->
            ( Dragging draggingItem
            , Just <|
                toSort
                    { listID = listID
                    , itemID = draggingItem.id
                    , index = 0
                    }
            )

        ( Dragging _, PointerUp ) ->
            ( Idle, Just toEnd )

        _ ->
            ( state, Nothing )


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


{-| subscribes to various global events
-}
subscriptions : State -> Sub Msg
subscriptions state =
    let
        positionToPosition pos =
            PointerMove (Vec2.fromTuple ( pos.clientX, pos.clientY ))
    in
        case state of
            Dragging _ ->
                Sub.batch
                    [ Mouse.ups (\_ -> PointerUp)
                    , Mouse.moves positionToPosition
                    ]

            _ ->
                Sub.none



-- View


{-| ViewDetails represent a set of attributes and child nodes
-}
type alias ViewDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| Config is the set of options for a sortable list.
-}
type Config item msg
    = Config
        { id : String
        , tag : String
        , attributes : List (Attribute msg)
        , itemTag : String
        , item : item -> ViewDetails msg
        , handle : Maybe String
        , toMsg : Msg -> msg
        , toID : item -> String
        }


{-| config returns a Config type.
-}
config :
    { id : String
    , tag : String
    , attributes : List (Attribute msg)
    , itemTag : String
    , item : item -> ViewDetails msg
    , handle : Maybe String
    , toMsg : Msg -> msg
    , toID : item -> String
    }
    -> Config item msg
config { id, tag, attributes, itemTag, item, handle, toMsg, toID } =
    Config
        { id = id
        , tag = tag
        , attributes = attributes
        , itemTag = itemTag
        , item = item
        , handle = handle
        , toMsg = toMsg
        , toID = toID
        }


{-| renders a list of sortable items.
-}
view : Config item msg -> State -> List item -> Html msg
view (Config config) state items =
    if List.isEmpty items then
        emptyListView (Config config) state
    else
        listView (Config config) state items


emptyListView : Config item msg -> State -> Html msg
emptyListView (Config config) state =
    let
        attributes =
            config.attributes
                ++ case state of
                    Dragging draggingItem ->
                        [ on "mouseover" <| Json.succeed <| config.toMsg <| PointerEmptyListIntersect config.id ]

                    Idle ->
                        []
    in
        Html.Keyed.node config.tag attributes []


listView : Config item msg -> State -> List item -> Html msg
listView (Config config) state items =
    let
        itemIDS =
            List.map config.toID items

        attributes =
            config.attributes

        itemChildren =
            case state of
                Dragging draggingItem ->
                    if draggingItem.hasDragged then
                        List.indexedMap
                            (\idx item ->
                                if config.toID item == draggingItem.id then
                                    draggingItemView (Config config) item
                                else
                                    siblingItemView (Config config) state idx item
                            )
                            items
                    else
                        List.map (idleItemView (Config config)) items

                Idle ->
                    List.map (idleItemView (Config config)) items

        cloneChildren =
            case state of
                Idle ->
                    []

                Dragging draggingItem ->
                    if draggingItem.hasDragged then
                        items
                            |> List.filter (\item -> config.toID item == draggingItem.id)
                            |> List.map (cloneView (Config config) draggingItem)
                    else
                        []

        children =
            itemChildren ++ cloneChildren
    in
        Html.Keyed.node config.tag attributes children


idleItemView : Config item msg -> item -> ( String, Html msg )
idleItemView (Config config) item =
    let
        id =
            config.toID item

        viewData =
            (config.item item)

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

        onMouseDown handle tagger =
            case handle of
                Just handle_ ->
                    onHandleMouseDown handle_ tagger

                Nothing ->
                    onItemMouseDown tagger

        attributes =
            viewData.attributes
                ++ [ class (itemClass id)
                   , Html.Attributes.map config.toMsg <| onMouseDown config.handle (PointerDown id)
                   ]

        children =
            viewData.children
    in
        ( id, Html.node config.itemTag attributes children )


draggingItemView : Config item msg -> item -> ( String, Html msg )
draggingItemView (Config config) item =
    let
        id =
            config.toID item

        viewData =
            (config.item item)

        attributes =
            viewData.attributes ++ [ class (itemClass id), style [ ( "opacity", "0.5" ) ] ]

        children =
            viewData.children
    in
        ( id, Html.node config.itemTag attributes children )


siblingItemView : Config item msg -> State -> Int -> item -> ( String, Html msg )
siblingItemView (Config config) state idx item =
    let
        id =
            config.toID item

        viewData =
            (config.item item)

        attributes =
            viewData.attributes
                ++ [ class (itemClass id)
                   , on "mousemove" (Json.succeed (PointerItemIntersect config.id id idx))
                        |> Html.Attributes.map config.toMsg
                   ]

        children =
            viewData.children
    in
        ( id, Html.node config.itemTag attributes children )


cloneView : Config item msg -> DraggingItem -> item -> ( String, Html msg )
cloneView (Config config) draggingItem item =
    let
        id =
            "clone-" ++ config.toID item

        viewData =
            config.item item

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

        attributes =
            viewData.attributes
                ++ [ style
                        [ ( "position", "fixed" )
                        , ( "top", px top )
                        , ( "left", px left )
                        , ( "width", px width )
                        , ( "height", px height )
                        , ( "z-index", "9999" )
                        , ( "pointer-events", "none" )
                        ]
                   ]

        children =
            viewData.children
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



-- Geometry Helpers


{-| Returns true if p2 intersects with the half of the bounds which is opposite
to the general direction of the movement from p1 to p2. For example, if the
direction of p1-p2 is "Right", the function will check for the intersection of
p2 and the right half of the bounds.
-}
detectSideIntersect : Position -> Position -> Rect -> Bool
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
getDirection : Position -> Position -> Direction
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
