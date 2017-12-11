module Bingo exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)

--main = 
--    Html.text (String.repeat 3 (String.toUpper "Another New Elm Application! "))

playerInfo : String -> String -> String
playerInfo name gameNumber =
    name ++ " Game number " ++ gameNumber

viewPlayer : String -> String -> Html msg 
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

view : Html msg
view =
    div [ class "content" ]
    [ viewHeader "MOFO Bingo",
      viewPlayer "Nicole" "5",
      viewFooter
    ]

main : Html msg
main =
   view
