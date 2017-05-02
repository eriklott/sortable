module Tiles exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Sortable


type alias Item =
    { id : String
    , name : String
    }


type alias Model =
    { items : List Item
    , sortable : Sortable.Model Item
    }


init : ( Model, Cmd Msg )
init =
    ( { items =
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
            , Item "11" "Strawberry"
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
    = SortableMsg (Sortable.Msg Item)


update : Msg -> Model -> Model
update msg model =
    case msg of
        SortableMsg msg_ ->
            let
                ( sortable_, _ ) =
                    Sortable.update msg_ model.sortable
            in
                { model | sortable = sortable_ }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SortableMsg (Sortable.subscriptions model.sortable)


view : Model -> Html Msg
view model =
    Sortable.list
        { toMsg = SortableMsg
        , toItemID = .id
        , id = "list1"
        , tag = "ul"
        , attributes = []
        , canReceiveItem = always True
        , canRemoveItem = always False
        , itemTag = "li"
        , itemDetails = itemView
        , itemHandle = Just "handle"
        }
        model.sortable
        model.items


itemView : Item -> Sortable.ViewDetails Msg
itemView item =
    { attributes = [ style [ ( "border", "1px solid black" ), ( "width", "100px" ), ( "height", "100px" ), ( "display", "inline-block" ) ] ]
    , children =
        [ div [ class "handle", style [ ( "background-color", "black" ), ( "width", "10px" ), ( "height", "10px" ) ] ] []
        , span [] [ text item.name ]
        ]
    }


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , update = (\msg model -> ( update msg model, Cmd.none ))
        , subscriptions = subscriptions
        , view = view
        }
