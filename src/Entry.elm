module Entry exposing (..)

import Dom
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Keyed as Keyed
import Json.Decode as Json
import Html.Events exposing (onInput, keyCode, on, onDoubleClick, onBlur)
import Html.Lazy exposing (lazy)
import Task exposing (perform)

-- MODEL 
type alias Model =
  { description : String
  , completed : Bool
  , editing : Bool
  , id : Int
  }


newEntry : String -> Int -> Model
newEntry desc id =
  { description = desc
  , completed = False
  , editing = False
  , id = id
  }

-- TRANSLATOR 
type alias Translator parentMsg =
  Msg -> parentMsg

type alias TranslationDictionary parentMsg = 
  { onInternalMsg : Int -> InternalMsg -> parentMsg 
  , onDeleteEntry : Int -> parentMsg 
  , onCheckAll : Bool -> parentMsg
  }

translator : TranslationDictionary parentMsg -> Translator parentMsg 
translator { onInternalMsg, onDeleteEntry, onCheckAll } msg = 
  case msg of 
    ForSelf id internal ->
      onInternalMsg id internal 

    ForParent (DeleteEntry id) -> 
      onDeleteEntry id

    ForParent (CheckAll bool) -> 
      onCheckAll bool


-- UPDATE 
type InternalMsg 
  = NoOp 
  | Edit Bool
  | Update String
  | Check Bool

type OutMsg 
  = DeleteEntry Int 
  | CheckAll Bool

type Msg 
  = ForSelf Int InternalMsg 
  | ForParent OutMsg

update : InternalMsg -> Model -> (Model, Cmd Msg) 
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    Edit isEditing -> 
      let
        focus = Dom.focus ("todo-" ++ toString id)
      in
        { model | editing = isEditing } 
        ! 
        [ perform (\_ -> ForSelf model.id NoOp) (\_ -> ForSelf model.id NoOp) focus ]

    Update str -> 
      { model | description = str } ! []

    Check isCompleted -> 
      { model | completed = isCompleted } ! []


-- VIEW 
view : String -> List Model -> Html Msg
view visibility entries =
  let
    isVisible todo =
      case visibility of
        "Completed" ->
            todo.completed

        "Active" ->
            not todo.completed

        _ ->
            True

    allCompleted =
      List.all .completed entries

    cssVisibility =
      if List.isEmpty entries 
      then "hidden"
      else "visible"

    entries' =
      entries 
        |> List.filter isVisible
        |> List.map viewKeyedEntry
        |> Keyed.ul [ class "todo-list" ]
          
  in
    section
        [ class "main"
        , style [ ( "visibility", cssVisibility ) ]
        ]
        [ input
            [ class "toggle-all"
            , type' "checkbox"
            , name "toggle"
            , checked allCompleted
            , onClick (CheckAll (not allCompleted) |> ForParent)
            ]
            []
        , label
            [ for "toggle-all" ]
            [ text "Mark all as complete" ]
        , entries'
        ]


viewKeyedEntry : Model -> ( String, Html Msg )
viewKeyedEntry todo =
  ( toString todo.id, lazy viewEntry todo )


viewEntry : Model -> Html Msg
viewEntry todo =
  li
    [ classList [ ( "completed", todo.completed ), ( "editing", todo.editing ) ] ]
    [ div
      [ class "view" ]
      [ input
          [ class "toggle"
          , type' "checkbox"
          , checked todo.completed
          , onClick (ForSelf todo.id (Check (not todo.completed)))
          ]
          []
      , label
          [ onDoubleClick (ForSelf todo.id (Edit True)) ]
          [ text todo.description ]
      , button
          [ class "destroy"
          , onClick (ForParent (DeleteEntry todo.id))
          ]
          []
      ]
    , input
      [ class "edit"
      , value todo.description
      , name "title"
      , id ("todo-" ++ toString todo.id)
      , onInput (ForSelf todo.id << Update)
      , onBlur (Edit False |> ForSelf todo.id)  
      , onEnter (Edit False |> ForSelf todo.id)
      ]
      []
    ]


onEnter : Msg -> Attribute Msg
onEnter msg =
  let
    isEnter code =
      if code == 13 
      then Json.succeed msg
      else Json.fail "not ENTER"
  in
    on "keydown" (Json.andThen keyCode isEnter)
