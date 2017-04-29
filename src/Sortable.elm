module Sortable exposing (Model, Event, Msg, ViewDetails, init, update, subscriptions, group, view)

{-| Sortable module provides the tools to easily create a sortable list.

# State
@docs Model, Event, Msg, init, update, subscriptions

# View Configuration
@docs ViewDetails, group, view

# View
@docs view
-}

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Html.Keyed
import Json.Decode as Json
import Dict exposing (Dict)
import BoundingBox
import Math.Vector2 as Vec2
import DOM
import Mouse


-- Model


{-| Model represents the state of the sortable list
-}
type Model
    = Idle SortState
    | Dragging SortState DraggingItem


type alias SortState =
    Dict String
        { listID : String
        , index : Int
        }


{-| DraggingItem contains state relating to the item currently being dragged
-}
type alias DraggingItem =
    { id : String
    , bounds : Rect
    , fromListID : String
    , fromIndex : Int
    , hasMoved : Bool
    , position : Point
    , prevPosition : Point
    }


{-| -}
type alias Event =
    { itemID : String
    , listID : String
    , index : Int
    , fromListID : String
    , fromIndex : Int
    }


{-| returns the initial state of the sortable list
-}
init : Model
init =
    Idle Dict.empty


sortState : Model -> SortState
sortState model =
    case model of
        Idle ss ->
            ss

        Dragging ss _ ->
            ss


updateSortState : String -> String -> Int -> SortState -> SortState
updateSortState itemID destListID destIndex sortState =
    case Dict.get itemID sortState of
        Just currentItem ->
            let
                decrementCurrentListIndices ss =
                    Dict.map
                        (\k { listID, index } ->
                            if listID == currentItem.listID && index > currentItem.index then
                                { listID = listID, index = index - 1 }
                            else
                                { listID = listID, index = index }
                        )
                        ss

                incrementDestListIndices ss =
                    Dict.map
                        (\k { listID, index } ->
                            if listID == destListID && index >= destIndex then
                                { listID = listID, index = index + 1 }
                            else
                                { listID = listID, index = index }
                        )
                        ss

                updateItem ss =
                    Dict.update itemID (Maybe.map (\v -> { v | listID = destListID, index = destIndex })) ss
            in
                sortState
                    |> decrementCurrentListIndices
                    |> incrementDestListIndices
                    |> updateItem

        Nothing ->
            sortState


newEvent : SortState -> DraggingItem -> Event
newEvent sortState draggingItem =
    { itemID = draggingItem.id
    , listID = Dict.get draggingItem.id sortState |> Maybe.map .listID |> Maybe.withDefault ""
    , index = Dict.get draggingItem.id sortState |> Maybe.map .index |> Maybe.withDefault 0
    , fromListID = draggingItem.fromListID
    , fromIndex = draggingItem.fromIndex
    }



-- Update


{-| Sortable list message
-}
type Msg
    = PointerDown String String Int Point
    | PointerMove Point
    | PointerOverItem String SortState
    | PointerOverList SortState
    | PointerUp


{-| updates the .sortable list model
-}
update : (Event -> msg) -> Msg -> Model -> ( Model, Maybe msg )
update onDragEnd msg model =
    case ( msg, model ) of
        ( PointerDown itemID listID index position, Idle sortState ) ->
            case getItemRectSync itemID of
                Just bounds ->
                    ( Dragging sortState
                        { id = itemID
                        , bounds = BoundingBox.translate (Vec2.negate position) bounds
                        , hasMoved = False
                        , fromListID = listID
                        , fromIndex = index
                        , position = position
                        , prevPosition = position
                        }
                    , Nothing
                    )

                Nothing ->
                    ( model, Nothing )

        ( PointerMove position, Dragging sortState draggingItem ) ->
            ( Dragging sortState
                { draggingItem
                    | position = position
                    , prevPosition = draggingItem.position
                    , hasMoved = draggingItem.hasMoved || draggingItem.position /= position
                }
            , Nothing
            )

        ( PointerOverItem targetItemID newSortState, Dragging sortState draggingItem ) ->
            case getItemRectSync targetItemID of
                Just bounds ->
                    if detectSideIntersect draggingItem.prevPosition draggingItem.position bounds then
                        ( Dragging newSortState draggingItem, Nothing )
                    else
                        ( model, Nothing )

                Nothing ->
                    ( model, Nothing )

        ( PointerOverList newSortState, Dragging sortState draggingItem ) ->
            ( Dragging newSortState draggingItem, Nothing )

        ( PointerUp, Dragging sortState draggingItem ) ->
            ( Idle sortState
            , Just <| onDragEnd <| newEvent sortState draggingItem
            )

        _ ->
            ( model, Nothing )



-- Subscriptions


{-| subscribes to various global events
-}
subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Idle _ ->
            Sub.none

        Dragging _ _ ->
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


type alias GroupConfig item msg =
    { toMsg : Msg -> msg
    , toID : item -> String
    , lists : List (ListConfig item msg)
    }


type alias ListConfig item msg =
    { id : String
    , tag : String
    , attributes : List (Attribute msg)
    , canReceiveItem : item -> Bool
    , canRemoveItem : item -> Bool
    , itemTag : String
    , itemDetails : item -> ViewDetails msg
    , itemHandle : Maybe String
    , items : List item
    }


sortGroupConfigLists : SortState -> GroupConfig item msg -> GroupConfig item msg
sortGroupConfigLists sortState groupConfig =
    groupConfig.lists
        |> List.foldr
            (\list ( intersection, rest ) ->
                List.foldr
                    (\item ( intersection, rest ) ->
                        case Dict.get (groupConfig.toID item) sortState of
                            Just { listID, index } ->
                                ( { item = item, listID = listID, index = index } :: intersection, rest )

                            Nothing ->
                                ( intersection, { item = item, listID = list.id, index = 0 } :: rest )
                    )
                    ( intersection, rest )
                    list.items
            )
            ( [], [] )
        |> (\( intersection, rest ) ->
                (List.sortBy .index intersection) ++ rest
           )
        |> List.foldr
            (\{ item, listID, index } dict ->
                if Dict.member listID dict then
                    Dict.update listID (Maybe.map (\items -> item :: items)) dict
                else
                    Dict.insert listID [ item ] dict
            )
            (Dict.empty)
        |> (\listDict ->
                List.map
                    (\listConfig -> { listConfig | items = Dict.get listConfig.id listDict |> Maybe.withDefault [] })
                    groupConfig.lists
           )
        |> (\lists -> { groupConfig | lists = lists })


groupConfigToDraggingItemViewModel : GroupConfig item msg -> Model -> Maybe (DraggingItemViewModel item msg)
groupConfigToDraggingItemViewModel groupConfig model =
    let
        findItem id items =
            case items of
                item :: rest ->
                    if (groupConfig.toID item == id) then
                        Just item
                    else
                        findItem id rest

                [] ->
                    Nothing

        findListAndItem id lists =
            case lists of
                list :: rest ->
                    case findItem id list.items of
                        Just item ->
                            Just ( list, item )

                        Nothing ->
                            findListAndItem id rest

                [] ->
                    Nothing
    in
        case model of
            Idle _ ->
                Nothing

            Dragging _ draggingItem ->
                case findListAndItem draggingItem.id groupConfig.lists of
                    Just ( list, item ) ->
                        Just
                            { item = item
                            , list = list
                            , bounds = draggingItem.bounds
                            , position = draggingItem.position
                            }

                    Nothing ->
                        Nothing


{-| -}
group : GroupConfig item msg -> Model -> ViewModel item msg
group groupConfig model =
    let
        groupConfig_ =
            sortGroupConfigLists (sortState model) groupConfig
    in
        { toMsg = groupConfig_.toMsg
        , toID = groupConfig_.toID
        , currentList = nullListViewModel
        , lists = groupConfig_.lists
        , draggingItem = groupConfigToDraggingItemViewModel groupConfig_ model
        }



-- View Model


type alias ListViewModel item msg =
    { id : String
    , tag : String
    , attributes : List (Attribute msg)
    , canReceiveItem : item -> Bool
    , canRemoveItem : item -> Bool
    , items : List item
    , itemTag : String
    , itemDetails : item -> ViewDetails msg
    , itemHandle : Maybe String
    }


type alias DraggingItemViewModel item msg =
    { item : item
    , list : ListViewModel item msg
    , bounds : Rect
    , position : Point
    }


type alias ViewModel item msg =
    { toMsg : Msg -> msg
    , toID : item -> String
    , currentList : ListViewModel item msg
    , lists : List (ListViewModel item msg)
    , draggingItem : Maybe (DraggingItemViewModel item msg)
    }


setViewModelCurrentList : String -> ViewModel item msg -> Maybe (ViewModel item msg)
setViewModelCurrentList listID viewModel =
    viewModel.lists
        |> find (\list -> list.id == listID)
        |> Maybe.map (\list -> { viewModel | currentList = list })


nullListViewModel : ListViewModel item msg
nullListViewModel =
    { id = ""
    , tag = ""
    , attributes = []
    , canReceiveItem = always False
    , canRemoveItem = always False
    , items = []
    , itemTag = ""
    , itemDetails = always (ViewDetails [] [])
    , itemHandle = Nothing
    }


viewModelToSortState : ViewModel item msg -> SortState
viewModelToSortState model =
    model.lists
        |> List.foldl
            (\list sortState ->
                list.items
                    |> List.foldl
                        (\item ( sortState, index ) ->
                            ( Dict.insert (model.toID item) { listID = list.id, index = index } sortState, index + 1 )
                        )
                        ( sortState, 0 )
                    |> Tuple.first
            )
            (Dict.empty)


{-| -}
view : ViewModel item msg -> String -> Html msg
view viewModel listID =
    setViewModelCurrentList listID viewModel
        |> Maybe.map listView
        |> Maybe.withDefault (text "")



-- List Views


listView : ViewModel item msg -> Html msg
listView model =
    case model.draggingItem of
        Just draggingItem ->
            if draggingItem.list.id /= model.currentList.id then
                if model.currentList.canReceiveItem draggingItem.item && draggingItem.list.canRemoveItem draggingItem.item then
                    enabledListView model draggingItem
                else
                    disabledListView model
            else
                enabledListView model draggingItem

        Nothing ->
            idleListView model


{-| The list view rendered when an item does not have permission to move to
this list
-}
disabledListView : ViewModel item msg -> Html msg
disabledListView model =
    let
        list =
            model.currentList

        children =
            List.map (disabledItemView model) list.items
    in
        Html.Keyed.node list.tag list.attributes children


{-| The list view rendered when an item can move to this list
-}
enabledListView : ViewModel item msg -> DraggingItemViewModel item msg -> Html msg
enabledListView model draggingItem =
    if List.isEmpty model.currentList.items then
        emptyListView model draggingItem
    else
        activeListView model draggingItem


{-| The list view rendered when there is an item actively being dragged
-}
activeListView : ViewModel item msg -> DraggingItemViewModel item msg -> Html msg
activeListView model draggingItem =
    let
        list =
            model.currentList

        itemView item ( children, index ) =
            if model.toID item == model.toID draggingItem.item then
                ( draggingItemView model item :: children ++ [ cloneItemView model draggingItem item ]
                , index - 1
                )
            else
                ( targetItemView model draggingItem index item :: children
                , index - 1
                )

        children =
            list.items
                |> List.foldr itemView ( [], (List.length list.items) - 1 )
                |> Tuple.first
    in
        Html.Keyed.node list.tag list.attributes children


{-| The list view rendered when an active list has no children
-}
emptyListView : ViewModel item msg -> DraggingItemViewModel item msg -> Html msg
emptyListView model draggingItem =
    let
        list =
            model.currentList

        pointerOverListMsg () =
            viewModelToSortState model
                |> updateSortState (model.toID draggingItem.item) list.id 0
                |> PointerOverList
                |> model.toMsg

        localAttributes =
            [ on "mouseover" <| Json.map pointerOverListMsg (Json.succeed ())
            ]

        attributes =
            list.attributes ++ localAttributes
    in
        Html.Keyed.node list.tag attributes []


{-| The list view rendered when there is no active dragging item
-}
idleListView : ViewModel item msg -> Html msg
idleListView model =
    let
        list =
            model.currentList

        children =
            List.indexedMap (idleItemView model) list.items
    in
        Html.Keyed.node list.tag list.attributes children



-- Item Views


disabledItemView : ViewModel item msg -> item -> ( String, Html msg )
disabledItemView model item =
    let
        id =
            model.toID item

        list =
            model.currentList

        itemDetails =
            list.itemDetails item
    in
        ( id, Html.node list.itemTag itemDetails.attributes itemDetails.children )


idleItemView : ViewModel item msg -> Int -> item -> ( String, Html msg )
idleItemView model index item =
    let
        id =
            model.toID item

        list =
            model.currentList

        itemDetails =
            list.itemDetails item

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
            case list.itemHandle of
                Just handle ->
                    onHandleMouseDown handle tagger

                Nothing ->
                    onItemMouseDown tagger

        localAttributes =
            [ class (itemClass id)
            , PointerDown id list.id index
                |> onMouseDown
                |> Html.Attributes.map model.toMsg
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node list.itemTag attributes children )


draggingItemView : ViewModel item msg -> item -> ( String, Html msg )
draggingItemView model item =
    let
        id =
            model.toID item

        list =
            model.currentList

        itemDetails =
            list.itemDetails item

        localAttributes =
            [ class (itemClass id)
            , style [ ( "opacity", "0.5" ) ]
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node list.itemTag attributes children )


targetItemView : ViewModel item msg -> DraggingItemViewModel item msg -> Int -> item -> ( String, Html msg )
targetItemView model draggingItem index item =
    let
        id =
            model.toID item

        list =
            model.currentList

        itemDetails =
            list.itemDetails item

        pointerOverItemMsg () =
            viewModelToSortState model
                |> updateSortState (model.toID draggingItem.item) list.id index
                |> PointerOverItem (model.toID item)
                |> model.toMsg

        onMouseMove =
            on "mousemove" (Json.map pointerOverItemMsg (Json.succeed ()))

        localAttributes =
            [ class (itemClass id)
            , onMouseMove
            ]

        attributes =
            itemDetails.attributes ++ localAttributes

        children =
            itemDetails.children
    in
        ( id, Html.node list.itemTag attributes children )


cloneItemView : ViewModel item msg -> DraggingItemViewModel item msg -> item -> ( String, Html msg )
cloneItemView model draggingItem item =
    let
        id =
            "clone-" ++ model.toID item

        list =
            model.currentList

        itemDetails =
            list.itemDetails item

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
        ( id, Html.node list.itemTag attributes children )



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
