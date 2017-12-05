module Bingo exposing (..)

import Html 

--main = 
--    Html.text (String.repeat 3 (String.toUpper "Another New Elm Application! "))

playerInfo name gameNumber =
    name ++ " Game number " ++ gameNumber

String -> number -> String
playerInfoText name gameNumber =
    playerInfo name gameNumber 
        |> String.toUpper
        |> Html.text
main = 
    playerInfoText "Zacck" 2 


