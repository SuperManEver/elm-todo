module Footer exposing (..)

import Html exposing (Html, footer, p, text, a)
import Html.Attributes exposing (class, href)

view : Html msg
view =
  footer [ class "info" ]
    [ p [] [ text "Double-click to edit a todo" ]
    , p []
        [ text "Written by "
        , a [ href "https://github.com/SuperManEver" ] [ text "Nikita Luparev" ]
        ]
    , p []
        [ text "Part of "
        , a [ href "http://todomvc.com" ] [ text "TodoMVC" ]
        ]
    ]
