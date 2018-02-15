module Bingo exposing (..)

import Html exposing (..)
import Random exposing(..)
import Html.Attributes exposing (..)
import Html.Events exposing(onClick)
import Http


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
  , entries = []
  }




-- UPDATE

type Msg = NewGame | Mark Int | NewRandom Int | NewEntries (Result Http.Error String) 

update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
    case msg of
      NewRandom  randomNumber ->
        ( { model | gameNumber = randomNumber }, Cmd.none )
      NewGame ->
        ( { model | gameNumber  = model.gameNumber + 1 }, getEntries )
      NewEntries (Ok jsonString) ->
        let
          _ = Debug.log "Wow this is cool!" jsonString
        in 
          (model, Cmd.none)
      NewEntries (Err error) ->
        let
          _ = Debug.log "Ouch" error
        in 
          (model, Cmd.none)
      Mark id ->
        let
          markEntry e =
            if e.id == id then
              { e | marked = (not e.marked)}
            else
              e
        in
          ( { model | entries = List.map markEntry model.entries }, Cmd.none )

-- COMMANDS

generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)
    
entriesUrl : String
entriesUrl = 
    "http://localhost:3000/random-entries"
    
getEntries : Cmd Msg
getEntries =
    entriesUrl 
        |> Http.getString
        |> Http.send NewEntries
  

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

sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
      entries
      |> List.filter .marked
      |> List.foldl(\entry sum -> sum + entry.points) 0

viewScore : Int -> Html Msg
viewScore sum =
    div
        [ class "score" ]
        [ span [ class "label"] [ text "Score" ]
        , span [ class "value"] [ text (toString sum) ]
        ]



view : Model -> Html Msg
view model =
    div [ class "content" ]
    [ viewHeader "MOFO Bingo"
    , viewPlayer model.name model.gameNumber
    , viewEntryList model.entries
    , viewScore (sumMarkedPoints model.entries)
    , div [ class "button-group" ]
          [ button [ onClick NewGame ] [ text "New Game"] ]
    , viewFooter
    ]

-- main : Html Msg
-- main =
--    view initialModel
main : Program Never Model Msg
main =
  Html.program
    { init =  ( initialModel, getEntries )
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }
