module Bingo exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)

-- MODEL 
initialModel = 
    { name = "Zacck"
    , gameNumber = 1 
    , entries = initialEntries
    }

initialEntries = [
    { id = 1, phrase = "Wacky Wack", points = 230, marked = False }
    , { id = 2, phrase = "Wacky Wacker", points = 240, marked = False }
    ]

-- VIEW

playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " Game number " ++ (toString gameNumber)

viewPlayer : String -> Int -> Html msg 
viewPlayer name gameNumber =
   let
    playerInfoText =
    playerInfo name gameNumber 
        |> String.toUpper
        |> text
   in   
    h2 [ id "info", class "classy"]
    [ playerInfoText ]

viewHeader : String -> Html msg
viewHeader title =
   header [] 
   [ h1 [] [ text title ] ]

viewFooter : Html msg
viewFooter =
   footer [] 
   [a [href "http://github.com/zacck"]
      [text "Built by Zacck"]
   ]

view model =
    div [ class "content" ]
    [ viewHeader "MOFO Bingo"
    , viewPlayer model.name model.gameNumber
    , div [ class "debug" ] [text (toString model)]
    , viewFooter
    ]

main : Html msg
main =
   view initialModel
