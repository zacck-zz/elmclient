module Bingo exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)

type alias Model = 
  { name  : String
  , gameNumber : Int 
  , entries: List Entry
  }
  
type alias Entry = 
  { id : Int
  , phrase : String
  , points : Int
  , marked :  Bool
  }
-- MODEL 
initialModel : Model
initialModel = 
    { name = "Zacck"
    , gameNumber = 1 
    , entries = initialEntries
    }
initialEntries : List Entry
initialEntries = 
    [ Entry  1 "Wacky Wack" 230 False
    , Entry 2 "Johny Cee" 240 False 
    , Entry 3 "Stella Pupp" 340 True
    , Entry 4 "Bowee Dee" 300 False
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
view : Model -> Html msg
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
