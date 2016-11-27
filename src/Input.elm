module Input exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Json.Decode as Json
import Html.Events exposing (onInput, keyCode, on)

-- MODEL 
type alias Model = String

initModel : String 
initModel = ""

-- TRANSLATOR 
type alias TranslationDictionary parentMsg = 
  { onInternalMsg : InternalMsg -> parentMsg
  , onAddEntry : String -> parentMsg
  }

type alias Translator parentMsg = 
  Msg -> parentMsg

translator : TranslationDictionary parentMsg -> Translator parentMsg 
translator { onInternalMsg, onAddEntry } msg = 
  case msg of 
    ForSelf internal -> 
      onInternalMsg internal 

    ForParent (AddEntry str) -> 
      onAddEntry str


-- UPDATE 

type InternalMsg 
  = NoOp 
  | UpdateField String
  | Clear 

type OutMsg 
  = AddEntry String 

type Msg 
  = ForSelf InternalMsg 
  | ForParent OutMsg 

update : InternalMsg -> Model -> (Model, Cmd Msg) 
update msg model = 
  case msg of 
    NoOp -> 
      model ! []

    UpdateField str -> 
      str ! []

    Clear -> 
      "" ! []

-- VIEW 
view : Model -> Html Msg
view model =
  header
    [ class "header" ]
    [ h1 [] [ text "todos" ]
    , input
      [ class "new-todo"
      , placeholder "What needs to be done?"
      , autofocus True
      , value model
      , name "newTodo"
      , onInput (ForSelf << UpdateField)
      , onEnter (ForParent (AddEntry model))
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