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
  
viewEntryItem : Entry -> Html msg
viewEntryItem item = 
  li []
    [ span [ class "phrase" ][ text item.phrase ]
    , span [ class "points" ][ text (toString item.points)]
    ]

viewEntryList : List Entry -> Html msg
viewEntryList entries = 
  let 
    entryItems = 
     List.map viewEntryItem entries
  in
    ul [] entryItems
  
view : Model -> Html msg
view model =
    div [ class "content" ]
    [ viewHeader "MOFO Bingo"
    , viewPlayer model.name model.gameNumber
    , viewEntryList model.entries
    , viewFooter
    ]

main : Html msg
main =
   view initialModel
