port module App exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.App as App
import Html.Events exposing (..)
import Html.Lazy exposing (lazy, lazy2)
import Json.Decode as Json
import String
import Task exposing (perform)

-- modules 
import Footer
import Input
import Entry

main =
  App.programWithFlags
    { init = init
    , view = view
    , update = updateWithStorage
    , subscriptions = \_ -> Sub.none
    }


port setStorage : Model -> Cmd msg


{-| We want to `setStorage` on every update. This function adds the setStorage
command for every step of the update function.
-}
updateWithStorage : Msg -> Model -> ( Model, Cmd Msg )
updateWithStorage msg model =
  let
    ( newModel, cmd ) = update msg model
  in
    newModel ! [ setStorage newModel, cmd ]


-- MODEL

type alias Model =
  { entries : List Entry.Model
  , field : Input.Model
  , uid : Int
  , visibility : String
  }



emptyModel : Model
emptyModel =
  { entries = []
  , visibility = "All"
  , field = Input.initModel
  , uid = 0
  }

init : Maybe Model -> ( Model, Cmd Msg )
init savedModel =
  Maybe.withDefault emptyModel savedModel ! []

-- TRANSLATORS

inputTranslator : Input.Translator Msg 
inputTranslator = 
  Input.translator 
    { onInternalMsg = InputMsg 
    , onAddEntry    = AddEntry
    }

entryTranslator : Entry.Translator Msg 
entryTranslator = 
  Entry.translator 
    { onInternalMsg = EntryMsg
    , onDeleteEntry = Delete
    , onCheckAll = CheckAll
    }


-- UPDATE

type Msg
  = NoOp
  | InputMsg Input.InternalMsg
  | EntryMsg Int Entry.InternalMsg
  | AddEntry String
  | Delete Int
  | DeleteComplete
  | CheckAll Bool
  | ChangeVisibility String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
  let 
    isEmpty str = 
      if String.isEmpty str 
      then model.entries
      else model.entries ++ [ Entry.newEntry str model.uid ]

    updateEntry id diff = 
      List.map (\ e -> if e.id == id then diff e else e)
  in
    case msg of
      NoOp ->
        model ! []

      AddEntry str ->
        let 
          (field', cmd) = Input.update Input.Clear model.field
        in
          { model | uid = model.uid + 1, entries = isEmpty str, field = field' } 
          ! 
          [ Cmd.map inputTranslator cmd ]
        
      Delete id ->
        { model | entries = List.filter (\t -> t.id /= id) model.entries } ! []


      DeleteComplete ->
        { model | entries = List.filter (not << .completed) model.entries } ! []
  

      CheckAll isCompleted ->
        let
          updateEntry t = { t | completed = isCompleted }
        in
          { model | entries = List.map updateEntry model.entries } ! []
       

      ChangeVisibility visibility ->
        { model | visibility = visibility } ! []


      InputMsg subMsg -> 
        let 
          (field', cmd) = Input.update subMsg model.field
        in
          { model | field = field' } ! [ Cmd.map inputTranslator cmd ]


      EntryMsg id subMsg -> 
        let 
          entry = 
            model.entries 
              |> List.filter (\ e -> e.id == id) 
              |> List.head 
        in          
          case entry of 
            Just val -> 
              let 
                (entry', cmd) = Entry.update subMsg val
                update = updateEntry id (\ _ -> entry') 
              in
                { model | entries = update model.entries } ! [ Cmd.map entryTranslator cmd ]

            Nothing -> 
              model ! []


-- VIEW

view : Model -> Html Msg
view model =
  div
    [ class "todomvc-wrapper"
    , style [ ( "visibility", "hidden" ) ]
    ]
    [ section
      [ class "todoapp" ]
      [ lazy Input.view model.field |> App.map inputTranslator
      , lazy2 Entry.view model.visibility model.entries |> App.map entryTranslator
      , lazy2 viewControls model.visibility model.entries
      ]
    , Footer.view
    ]

-- VIEW CONTROLS 

viewControls : String -> List Entry.Model -> Html Msg
viewControls visibility entries =
  let
    entriesCompleted =
      List.length (List.filter .completed entries)

    entriesLeft =
      List.length entries - entriesCompleted
  in
    footer
      [ class "footer"
      , hidden (List.isEmpty entries)
      ]
      [ lazy viewControlsCount entriesLeft
      , lazy viewControlsFilters visibility
      , lazy viewControlsClear entriesCompleted
      ]


viewControlsCount : Int -> Html Msg
viewControlsCount entriesLeft =
  let
    item_ =
      if entriesLeft == 1 
      then " item"
      else " items"
  in
    span
      [ class "todo-count" ]
      [ strong [] [ text (toString entriesLeft) ]
      , text (item_ ++ " left")
      ]


viewControlsFilters : String -> Html Msg
viewControlsFilters visibility =
  ul
    [ class "filters" ]
    [ visibilitySwap "#/" "All" visibility
    , text " "
    , visibilitySwap "#/active" "Active" visibility
    , text " "
    , visibilitySwap "#/completed" "Completed" visibility
    ]


visibilitySwap : String -> String -> String -> Html Msg
visibilitySwap uri visibility actualVisibility =
  li
    [ onClick (ChangeVisibility visibility) ]
    [ a [ href uri, classList [ ( "selected", visibility == actualVisibility ) ] ]
        [ text visibility ]
    ]


viewControlsClear : Int -> Html Msg
viewControlsClear entriesCompleted =
  button
    [ class "clear-completed"
    , hidden (entriesCompleted == 0)
    , onClick DeleteComplete
    ]
    [ text ("Clear completed (" ++ toString entriesCompleted ++ ")")
    ]

