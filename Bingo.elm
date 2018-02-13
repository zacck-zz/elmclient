module Bingo exposing (..)

import Html exposing (..) 
import Html.Attributes exposing (..)
import Html.Events exposing(onClick)


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
    List.sortBy .points
    [ Entry  1 "Wacky Wack" 230 False
    , Entry 2 "Johny Cee" 220 False 
    , Entry 3 "Stella Pupp" 3400 True
    , Entry 4 "Bowee Dee" 300 False
    ]
    
    
    
-- UPDATE 

type Msg = NewGame | Mark Int

update : Msg -> Model -> Model
update msg model = 
    case msg of 
      NewGame -> 
        { model | gameNumber = model.gameNumber + 1, 
                  entries = initialEntries }
      Mark id ->
        let 
          markEntry e = 
            if e.id == id then 
              { e | marked = (not e.marked)}
            else
              e  
        in
          { model | entries = List.map markEntry model.entries }

-- VIEW

playerInfo : String -> Int -> String
playerInfo name gameNumber =
    name ++ " Game number " ++ (toString gameNumber)

viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
   let
    playerInfoText =
    playerInfo name gameNumber 
        |> String.toUpper
        |> text
   in   
    h2 [ id "info", class "classy"]
    [ playerInfoText ]

viewHeader : String -> Html Msg
viewHeader title =
   header [] 
   [ h1 [] [ text title ] ]

viewFooter : Html Msg
viewFooter =
   footer [] 
   [a [href "http://github.com/zacck"]
      [text "Built by Zacck"]
   ]
  
viewEntryItem : Entry -> Html Msg
viewEntryItem item = 
  li [  classList [ ("marked", item.marked) ], onClick (Mark item.id) ]
    [ span [ class "phrase" ][ text item.phrase ]
    , span [ class "points" ][ text (toString item.points)]
    ]

viewEntryList : List Entry -> Html Msg
viewEntryList entries = 
  let 
    entryItems = 
     List.map viewEntryItem entries
  in
    ul [] entryItems
  
view : Model -> Html Msg
view model =
    div [ class "content" ]
    [ viewHeader "MOFO Bingo"
    , viewPlayer model.name model.gameNumber
    , viewEntryList model.entries
    , div [ class "button-group" ]
          [ button [ onClick NewGame ] [ text "New Game"] ]
    , viewFooter
    ]

-- main : Html Msg
-- main =
--    view initialModel
main : Program Never Model Msg 
main = 
  Html.beginnerProgram
    { model =  initialModel
    , view = view
    , update = update
    }
