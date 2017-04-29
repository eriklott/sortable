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
    , sortable : Sortable.Model
    }


init : ( Model, Cmd Msg )
init =
    ( { items1 =
            [ Item "01" "Apple"
            , Item "02" "Orange"
            , Item "03" "Grape"
            , Item "04" "Banana"
            , Item "05" "Melon"
            , Item "06" "Strawberry"
            , Item "07" "Orange"
            , Item "08" "Grape"
            , Item "09" "Banana"
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
    | End Sortable.Event


update : Msg -> Model -> Model
update msg model =
    case msg of
        SortableMsg msg_ ->
            let
                ( sortable_, _ ) =
                    Debug.log "response" <| Sortable.update End msg_ model.sortable
            in
                { model | sortable = sortable_ }

        End _ ->
            model


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.map SortableMsg (Sortable.subscriptions model.sortable)


view : Model -> Html Msg
view model =
    let
        sortableConfig =
            Sortable.group
                { toMsg = SortableMsg
                , toID = .id
                , lists =
                    [ { id = "list1"
                      , tag = "ul"
                      , attributes = []
                      , canReceiveItem = always True
                      , canRemoveItem = always True
                      , itemTag = "li"
                      , itemDetails = itemView
                      , itemHandle = Nothing
                      , items = model.items1
                      }
                    , { id = "list2"
                      , tag = "ol"
                      , attributes = []
                      , canReceiveItem = always True
                      , canRemoveItem = always True
                      , itemTag = "li"
                      , itemDetails = itemView
                      , itemHandle = Nothing
                      , items = model.items2
                      }
                    , { id = "list3"
                      , tag = "ul"
                      , attributes = [ style [ ( "border", "1px solid black" ), ( "width", "400px" ), ( "min-height", "400px" ) ] ]
                      , canReceiveItem = always True
                      , canRemoveItem = always False
                      , itemTag = "li"
                      , itemDetails = itemView
                      , itemHandle = Nothing
                      , items = []
                      }
                    ]
                }
                model.sortable
    in
        div []
            [ Sortable.view sortableConfig "list1"
            , Sortable.view sortableConfig "list2"
            , Sortable.view sortableConfig "list3"
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
