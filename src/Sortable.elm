module Sortable exposing (Model, Msg, ViewDetails, init, update, subscriptions, group, view)

{-| Sortable module provides the tools to easily create a sortable list.

# State
@docs Model, Msg, init, update, subscriptions

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
    case msg of
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



-- View Models


{-| ViewDetails represent a set of attributes and child nodes
-}
type alias ViewDetails msg =
    { attributes : List (Attribute msg)
    , children : List (Html msg)
    }


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
    , draggingItem :
        Maybe (DraggingItemViewModel item msg)
    }


nullListView : ListViewModel item msg
nullListView =
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
                            ( Dict.insert (model.toID item) ( list.id, index ) sortState, index + 1 )
                        )
                        ( sortState, 0 )
                    |> Tuple.first
            )
            (Dict.empty)


{-| -}
group :
    { toMsg : Msg -> msg
    , toID : item -> String
    , lists :
        List
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
    }
    -> Model
    -> ViewModel item msg
group config (Model model) =
    let
        listIDS =
            config.lists
                |> List.map .id
                |> Set.fromList

        sortedListItems : Dict String (List item)
        sortedListItems =
            config.lists
                |> List.foldr
                    (\list ( intersection, rest ) ->
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

        lists =
            config.lists
                |> List.map
                    (\list -> { list | items = Dict.get list.id sortedListItems |> Maybe.withDefault [] })

        currentItem itemID =
            lists
                |> List.foldr
                    (\list out ->
                        case out of
                            Just _ ->
                                out

                            Nothing ->
                                case find (\item -> config.toID item == itemID) list.items of
                                    Just item ->
                                        Just ( list.id, item )

                                    Nothing ->
                                        Nothing
                    )
                    Nothing

        draggingItem =
            case model.draggingItem of
                Just draggingItem ->
                    if draggingItem.hasDragged then
                        case currentItem draggingItem.id of
                            Just ( listID, item ) ->
                                Just
                                    { item = item
                                    , list =
                                        lists
                                            |> find (\list -> list.id == draggingItem.fromListID)
                                            |> Maybe.withDefault nullListView
                                    , bounds = draggingItem.bounds
                                    , position = draggingItem.currentPos
                                    }

                            Nothing ->
                                Nothing
                    else
                        Nothing

                Nothing ->
                    Nothing
    in
        { toMsg = config.toMsg
        , toID = config.toID
        , currentList = nullListView
        , lists = lists
        , draggingItem = draggingItem
        }



-- View


{-| -}
view : ViewModel item msg -> String -> Html msg
view view listID =
    let
        listWithID id list =
            list.id == id

        setViewModelCurrentList view list =
            { view | currentList = list }

        renderNothing =
            text ""
    in
        view.lists
            |> find (listWithID listID)
            |> Maybe.map (setViewModelCurrentList view)
            |> Maybe.map listView
            |> Maybe.withDefault renderNothing



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
        emptyListView model
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
                ( targetItemView model index item :: children
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
emptyListView : ViewModel item msg -> Html msg
emptyListView model =
    let
        list =
            model.currentList

        localAttributes =
            [ PointerOverEmptyList list.id
                |> model.toMsg
                |> Json.succeed
                |> on "mouseover"
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
            List.map (idleItemView model) list.items
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


idleItemView : ViewModel item msg -> item -> ( String, Html msg )
idleItemView model item =
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

        lazyPointerDownTagger p =
            PointerDown (viewModelToSortState model) list.id id p

        localAttributes =
            [ class (itemClass id)
            , lazyPointerDownTagger
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


targetItemView : ViewModel item msg -> Int -> item -> ( String, Html msg )
targetItemView model index item =
    let
        id =
            model.toID item

        list =
            model.currentList

        itemDetails =
            list.itemDetails item

        localAttributes =
            [ class (itemClass id)
            , on "mousemove" (Json.succeed (PointerOverTargetItem list.id id index))
                |> Html.Attributes.map model.toMsg
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
