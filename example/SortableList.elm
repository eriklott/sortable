module SortableList exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Sortable


type alias Item =
    { id : String
    , name : String
    }


type alias Model =
    { items1 : List Item
    , items2 : List Item
    , sortable : Sortable.State
    }


init : ( Model, Cmd Msg )
init =
    ( { items1 =
            [ Item "1" "Apple"
            , Item "2" "Orange"
            , Item "3" "Grape"
            , Item "4" "Banana"
            , Item "5" "Melon"
            , Item "6" "Strawberry"
            , Item "7" "Orange"
            , Item "8" "Grape"
            , Item "9" "Banana"
            , Item "10" "Melon"
            ]
      , items2 =
            [ Item "11" "Strawberry"
            , Item "12" "Orange"
            , Item "13" "Grape"
            , Item "14" "Banana"
            , Item "15" "Melon"
            , Item "16" "Strawberry"
            , Item "17" "Orange"
            , Item "18" "Grape"
            , Item "19" "Banana"
            , Item "20" "Melon"
            , Item "21" "Strawberry"
            ]
      , sortable = Sortable.init
      }
    , Cmd.none
    )


type Msg
    = SortableMsg Sortable.Msg
    | Sort Sortable.Sort
    | NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        SortableMsg msg_ ->
            let
                ( sortable, outMsg ) =
                    Sortable.update Sort NoOp msg_ model.sortable

                model_ =
                    { model | sortable = sortable }
            in
                case outMsg of
                    Just outMsg_ ->
                        update outMsg_ model_

                    Nothing ->
                        model_

        Sort sort ->
            let
                item =
                    List.filter (\itm -> itm.id == sort.itemID) model.items1

                filteredItems =
                    List.filter (\itm -> itm.id /= sort.itemID) model.items1

                sortedItems =
                    List.take sort.index filteredItems ++ item ++ List.drop sort.index filteredItems
            in
                { model | items1 = sortedItems }

        NoOp ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SortableMsg (Sortable.subscriptions model.sortable)


view : Model -> Html Msg
view model =
    div []
        [ Sortable.list (Sortable.config "list1" "ul" [] "li" itemView Nothing SortableMsg .id) model.sortable model.items1
        , Sortable.list (Sortable.config "list2" "ul" [] "li" itemView Nothing SortableMsg .id) model.sortable model.items2
        ]


itemView : Item -> Sortable.ViewDetails Msg
itemView item =
    { attributes = [ style [ ( "border", "1px solid black" ), ( "width", "400px" ) ] ]
    , children = [ text item.name ]
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = (\msg model -> ( update msg model, Cmd.none ))
        , subscriptions = subscriptions
        , view = view
        }
