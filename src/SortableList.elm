module SortableList exposing (Model, Event, Msg, ViewDetails, ListConfig, init, update, subscriptions, list)

{-| SortableList module provides the tools to easily create a sortable list.

# State
@docs Model, Event, Msg, init, update, subscriptions

# View Configuration
@docs ViewDetails, ListConfig

# View
@docs list
-}

import BoundingBox
import DOM
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as Json
import Math.Vector2 as Vec2
import Mouse


-- Model


{-| Model represents the state of the sortable list
-}
type Model item
    = Idle
    | Dragging (DraggingItem item)


{-| DraggingItem contains state relating to the item currently being dragged
-}
type alias DraggingItem item =
    { id : String
    , item : item
    , listID : String
    , index : Int
    , fromListID : String
    , fromIndex : Int
    , canRemoveFromList : Bool
    , bounds : Rect
    , hasMoved : Bool
    , position : Point
    , previousPosition : Point
    }


{-| Events are returned at specific time in the sortable list life cycle.
-}
type alias Event =
    { itemID : String
    , listID : String
    , index : Int
    , fromListID : String
    , fromIndex : Int
    }


{-| Returns the initial model of the sortable list
-}
init : Model item
init =
    Idle



-- Update


{-| Sortable list message
-}
type Msg item
    = PointerDown
        { item : item
        , id : String
        , listID : String
        , index : Int
        , canRemoveFromList : Bool
        , position : Point
        }
    | PointerMove Point
    | PointerOverItem String String Int
    | PointerOverList String
    | PointerUp


{-| Update the sortable list
-}
update : (Event -> msg) -> Msg item -> Model item -> ( Model item, Maybe msg )
update onDragEnd msg model =
    case msg of
        PointerDown { item, id, listID, index, canRemoveFromList, position } ->
            case model of
                Idle ->
                    case getItemRectSync id of
                        Just bounds ->
                            ( Dragging
                                { item = item
                                , id = id
                                , listID = listID
                                , index = index
                                , fromListID = listID
                                , fromIndex = index
                                , canRemoveFromList = canRemoveFromList
                                , bounds = BoundingBox.translate (Vec2.negate position) bounds
                                , hasMoved = False
                                , position = position
                                , previousPosition = position
                                }
                            , Nothing
                            )

                        Nothing ->
                            ( model, Nothing )

                Dragging _ ->
                    ( model, Nothing )

        PointerMove position ->
            case model of
                Idle ->
                    ( model, Nothing )

                Dragging draggingItem ->
                    ( Dragging
                        { draggingItem
                            | position = position
                            , previousPosition = draggingItem.position
                            , hasMoved = draggingItem.hasMoved || draggingItem.position /= position
                        }
                    , Nothing
                    )

        PointerOverItem id listID index ->
            case model of
                Idle ->
                    ( model, Nothing )

                Dragging draggingItem ->
                    case getItemRectSync id of
                        Just bounds ->
                            if detectSideIntersect draggingItem.previousPosition draggingItem.position bounds then
                                ( Dragging { draggingItem | listID = listID, index = index }, Nothing )
                            else
                                ( model, Nothing )

                        Nothing ->
                            ( model, Nothing )

        PointerOverList listID ->
            case model of
                Idle ->
                    ( model, Nothing )

                Dragging draggingItem ->
                    ( Dragging { draggingItem | listID = listID, index = 0 }, Nothing )

        PointerUp ->
            case model of
                Idle ->
                    ( Idle, Nothing )

                Dragging draggingItem ->
                    ( Idle
                    , Just <|
                        onDragEnd
                            { itemID = draggingItem.id
                            , listID = draggingItem.listID
                            , index = draggingItem.index
                            , fromListID = draggingItem.fromListID
                            , fromIndex = draggingItem.fromIndex
                            }
                    )



-- Subscriptions


{-| Sortable list subscriptions.
-}
subscriptions : Model item -> Sub (Msg item)
subscriptions model =
    case model of
        Idle ->
            Sub.none

        Dragging _ ->
            Sub.batch
                [ Mouse.moves (\p -> PointerMove p.clientXY)
                , Mouse.ups (\_ -> PointerUp)
                ]



-- View Config


{-| ViewDetails represent a set of attributes and child nodes
-}
type alias ViewDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


{-| The configuration for a sortable list
-}
type alias ListConfig item msg =
    { toMsg : Msg item -> msg
    , toItemID : item -> String
    , id : String
    , tag : String
    , attributes : List (Attribute msg)
    , canReceiveItem : item -> Bool
    , canRemoveItem : item -> Bool
    , itemTag : String
    , itemDetails : item -> ViewDetails msg
    , itemHandle : Maybe String
    }


{-| Returns a rendered sortable list
-}
list : ListConfig item msg -> Model item -> List item -> Html msg
list config model items =
    case model of
        Idle ->
            idleListView config items

        Dragging draggingItem ->
            let
                itemsWithoutDraggingItem =
                    List.filter (\item -> config.toItemID item /= draggingItem.id) items
            in
                if draggingItem.fromListID /= config.id then
                    if draggingItem.canRemoveFromList && config.canReceiveItem draggingItem.item then
                        draggingListView config draggingItem itemsWithoutDraggingItem
                    else
                        disabledListView config itemsWithoutDraggingItem
                else
                    draggingListView config draggingItem itemsWithoutDraggingItem


{-| The list rendered when the sortable list state is 'idle'
-}
idleListView : ListConfig item msg -> List item -> Html msg
idleListView config items =
    let
        children =
            List.indexedMap (idleItemView config) items
    in
        Html.Keyed.node config.tag config.attributes children


{-| The list item rendered when the sortable list state is 'idle'
-}
idleItemView : ListConfig item msg -> Int -> item -> ( String, Html msg )
idleItemView config index item =
    let
        id =
            config.toItemID item

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
            , onMouseDown
                (\position ->
                    config.toMsg <|
                        PointerDown
                            { item = item
                            , id = id
                            , listID = config.id
                            , index = index
                            , canRemoveFromList = config.canRemoveItem item
                            , position = position
                            }
                )
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        itemDetails =
            config.itemDetails item
    in
        ( id, Html.node config.itemTag attributes itemDetails.children )


{-| The list rendered when the currently dragging item cannot interact with
this list.
-}
disabledListView : ListConfig item msg -> List item -> Html msg
disabledListView config items =
    let
        children =
            List.map (disabledItemView config) items
    in
        Html.Keyed.node config.tag config.attributes children


{-| The list item rendered when the currently dragging item cannot interact with
this list.
-}
disabledItemView : ListConfig item msg -> item -> ( String, Html msg )
disabledItemView config item =
    let
        id =
            config.toItemID item

        itemDetails =
            config.itemDetails item
    in
        ( id, Html.node config.itemTag itemDetails.attributes itemDetails.children )


{-| The list rendered when the state of the sortable list is 'dragging' and
the dragging item can interact with this list.
-}
draggingListView : ListConfig item msg -> DraggingItem item -> List item -> Html msg
draggingListView config draggingItem items =
    let
        targetItemChildren items children index =
            if draggingItem.listID == config.id && draggingItem.index == index then
                targetItemChildren items
                    (children ++ [ placeholderItemView config draggingItem ])
                    (index + 1)
            else
                case items of
                    item :: rest ->
                        targetItemChildren rest
                            (children ++ [ targetItemView config index item ])
                            (index + 1)

                    [] ->
                        children

        cloneItemChildren =
            if draggingItem.fromListID == config.id then
                [ cloneItemView config draggingItem ]
            else
                []

        localAttributes =
            if List.isEmpty items then
                [ on "mouseover" <| Json.succeed <| config.toMsg <| PointerOverList config.id ]
            else
                []

        attributes =
            config.attributes ++ localAttributes

        children =
            (targetItemChildren items [] 0) ++ cloneItemChildren
    in
        Html.Keyed.node config.tag attributes children


{-| The item rendered in a draggingListView that is a sibling of currently
dragging item.
-}
targetItemView : ListConfig item msg -> Int -> item -> ( String, Html msg )
targetItemView config index item =
    let
        id =
            config.toItemID item

        itemDetails =
            config.itemDetails item

        onMouseMove tagger =
            onWithOptions "mousemove"
                { defaultOptions | preventDefault = True, stopPropagation = True }
                (Json.succeed tagger)

        localAttributes =
            [ class (itemClass id)
            , onMouseMove <| config.toMsg <| PointerOverItem id config.id index
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node config.itemTag attributes children )


{-| The placeholder for the currently dragging item that is rendered in a
draggingListView.
-}
placeholderItemView : ListConfig item msg -> DraggingItem item -> ( String, Html msg )
placeholderItemView config draggingItem =
    let
        id =
            "placeholder-" ++ draggingItem.id

        itemDetails =
            config.itemDetails draggingItem.item

        localAttributes =
            [ style
                [ ( "visibility", "hidden" )
                ]
            ]

        attributes =
            itemDetails.attributes

        children =
            itemDetails.children
    in
        ( id, Html.node config.itemTag attributes children )


{-| The clone (the item that follows the mouse drag) rendered for the currently
dragging item in a draggingListView.
-}
cloneItemView : ListConfig item msg -> DraggingItem item -> ( String, Html msg )
cloneItemView config draggingItem =
    let
        id =
            "clone-" ++ draggingItem.id

        itemDetails =
            config.itemDetails draggingItem.item

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
                , ( "list-style", "none" )
                ]
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node config.itemTag attributes children )



-- Event Decoders


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



-- Geometry Helpers


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
