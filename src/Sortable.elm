module Sortable exposing (State, Msg, Item, init, update, subscriptions, item, li, list, ol, ul, isDragging, insertAt)

{-| Sortable provides list sorting functionality

# Types
@docs State, Msg, Item

# Infrastructure
@docs init, update, subscriptions

# Helpers
@docs isDragging, insertAt

# Sorting
@docs list, ol, ul, item, li
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed exposing (..)
import Mouse
import Json.Decode as Decode
import DOM


-- Model


{-| State represents the state of the sort
-}
type State
    = Idle
    | Delayed DraggingModel
    | Dragging DraggingModel


{-| DraggingModel represents the model of the current drag
-}
type alias DraggingModel =
    { itemID : String
    , itemRect : Rectangle
    , dragStart : Mouse.Position
    , dragCurrent : Mouse.Position
    }


{-| Rectangle represents the dimensions of a dom node
-}
type alias Rectangle =
    { top : Int
    , left : Int
    , width : Int
    , height : Int
    }


{-| The initial state of a sortable list
-}
init : State
init =
    Idle


{-| isDragging returns true when a list item is currently being dragged.
-}
isDragging : State -> Bool
isDragging state =
    case state of
        Dragging _ ->
            True

        _ ->
            False



-- Update


{-| Msg represents a sortable list message
-}
type Msg
    = MouseDown String Mouse.Position Rectangle
    | MouseMove Mouse.Position
    | MouseUp


{-| update returns a new state
-}
update : Msg -> State -> State
update msg state =
    case ( state, msg ) of
        ( Idle, MouseDown itemID mousePos itemRect ) ->
            Delayed
                { itemID = itemID
                , itemRect = itemRect
                , dragStart = mousePos
                , dragCurrent = mousePos
                }

        ( Delayed model, MouseMove mousePos ) ->
            Dragging { model | dragCurrent = mousePos }

        ( Dragging model, MouseUp ) ->
            Idle

        _ ->
            (state)



-- Subscriptions


{-| subscribes to various global events
-}
subscriptions : State -> Sub Msg
subscriptions state =
    case state of
        Dragging _ ->
            Sub.batch
                [ Mouse.moves MouseMove
                , Mouse.ups (\_ -> MouseUp)
                ]

        _ ->
            Sub.none



-- Item Node


{-| Item represents an item in the sortable list
-}
type Item msg
    = Item
        { tag : String
        , id : String
        , attributes : List (Attribute msg)
        , children : List (Html msg)
        }


{-| creates a list item tag of a specified type.
-}
item : String -> String -> List (Attribute msg) -> List (Html msg) -> Item msg
item tag id attributes children =
    Item
        { tag = tag
        , id = id
        , attributes = attributes
        , children = children
        }


{-| creates an li list item tag.
-}
li : String -> List (Attribute msg) -> List (Html msg) -> Item msg
li id attributes children =
    item "li" id attributes children



-- List Node


{-| creates a list tag of a specified type.
-}
list : (Msg -> msg) -> (String -> String -> Int -> msg) -> String -> State -> String -> List (Attribute msg) -> List (Item msg) -> Html msg
list msg onMove id state tag attributes items =
    let
        itemNodes =
            (List.indexedMap (itemView msg onMove state id) items)

        cloneNode : List ( String, Html msg )
        cloneNode =
            case state of
                Dragging model ->
                    items
                        |> List.filter (\(Item item) -> item.id == model.itemID)
                        |> List.map (cloneView model)

                _ ->
                    []

        childNodes =
            itemNodes ++ cloneNode
    in
        Html.Keyed.node tag attributes childNodes


{-| creates an ol list tag.
-}
ol : (Msg -> msg) -> (String -> String -> Int -> msg) -> String -> State -> List (Attribute msg) -> List (Item msg) -> Html msg
ol msg onMove id model attributes items =
    list msg onMove id model "ol" attributes items


{-| creates an ul list tag.
-}
ul : (Msg -> msg) -> (String -> String -> Int -> msg) -> String -> State -> List (Attribute msg) -> List (Item msg) -> Html msg
ul msg onMove id model attributes items =
    list msg onMove id model "ul" attributes items


itemView : (Msg -> msg) -> (String -> String -> Int -> msg) -> State -> String -> Int -> Item msg -> ( String, Html msg )
itemView msg onMove state listID index (Item item) =
    let
        draggingItemAttributes =
            [ style
                [ ( "opacity", "0.5" )
                ]
            ]

        siblingItemAttributes draggingModel =
            [ onMouseMove <| onMove listID draggingModel.itemID index
            ]

        idleItemAttributes =
            [ Html.Attributes.map msg (onMouseDown (MouseDown item.id))
            , style [ ( "cursor", "move" ) ]
            ]

        attributes =
            item.attributes
                ++ case state of
                    Dragging model ->
                        if item.id == model.itemID then
                            draggingItemAttributes
                        else
                            siblingItemAttributes model

                    _ ->
                        idleItemAttributes
    in
        ( item.id, Html.node item.tag attributes item.children )


cloneView : DraggingModel -> Item msg -> ( String, Html msg )
cloneView model (Item item) =
    let
        dragDeltaX =
            model.dragCurrent.x - model.dragStart.x

        dragDeltaY =
            model.dragCurrent.y - model.dragStart.y

        left =
            model.itemRect.left + dragDeltaX

        top =
            model.itemRect.top + dragDeltaY

        cloneAttributes =
            [ style
                [ ( "position", "absolute" )
                , ( "top", px top )
                , ( "left", px left )
                , ( "width", px model.itemRect.width )
                , ( "height", px model.itemRect.height )
                , ( "z-index", "9999" )
                , ( "pointer-events", "none" )
                ]
            ]
    in
        ( "clone-" ++ item.id, Html.node item.tag (item.attributes ++ cloneAttributes) item.children )


px : v -> String
px v =
    toString v ++ "px"



-- Events


onMouseDown : (Mouse.Position -> Rectangle -> msg) -> Attribute msg
onMouseDown tagger =
    on "mousedown" (Decode.map2 tagger Mouse.position targetOffsetRect)


onMouseMove : msg -> Html.Attribute msg
onMouseMove tagger =
    on "mousemove" (Decode.succeed tagger)


targetOffsetRect : Decode.Decoder Rectangle
targetOffsetRect =
    DOM.target <| offsetRect


offsetRect : Decode.Decoder Rectangle
offsetRect =
    Decode.map4 Rectangle
        (Decode.map round DOM.offsetTop)
        (Decode.map round DOM.offsetLeft)
        (Decode.map round DOM.offsetWidth)
        (Decode.map round DOM.offsetHeight)



-- utils


{-| inserts a set of items into a list at a specified index
-}
insertAt : Int -> List a -> List a -> List a
insertAt index items list =
    let
        pushIdx =
            if index == -1 then
                List.length list
            else
                index

        idx =
            Basics.min (List.length list) (Basics.max pushIdx 0)
    in
        List.append (List.take idx list) (List.append items (List.drop idx list))
