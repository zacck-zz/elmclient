module Bingo exposing (..)

import Html exposing (..)
import Random exposing(..)
import Html.Attributes exposing (..)
import Html.Events exposing(onClick)
import Http
import Json.Decode as Decode exposing(Decoder, field, succeed)
import Json.Encode as Encode



type alias Model =
  { name  : String
  , gameNumber : Int
  , entries: List Entry
  , alertMessage: Maybe String
  }

type alias Entry =
  { id : Int
  , phrase : String
  , points : Int
  , marked :  Bool
  }
type alias Score =
  { id : Int
  , name: String
  , score: Int
  }

-- MODEL
initialModel : Model
initialModel =
  { name = "Zacck"
  , gameNumber = 1
  , entries = []
  , alertMessage = Nothing
  }




-- UPDATE

type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)

update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
    case msg of
      NewRandom  randomNumber ->
        ( { model | gameNumber = randomNumber }, Cmd.none )
      ShareScore ->
        ( model, postScore model )
      NewScore (Ok score) ->
          let
            message =
              "Your Score of "
                  ++ (toString score.score)
                  ++ " was shared successfully"
          in
            ( { model | alertMessage = Just message }, Cmd.none)
      NewScore (Err error) ->
          let
            message =
              "Error posting your score "
                  ++ (toString error)
          in
            ( { model | alertMessage = Just message }, Cmd.none)
      NewGame ->
        ( { model | gameNumber  = model.gameNumber + 1 }, getEntries )
      NewEntries (Ok randomEntries) ->
          ({ model | entries = List.sortBy .points randomEntries }, Cmd.none)
      NewEntries (Err error) ->
        let
          errorMessage =
            case error of
              Http.NetworkError ->
                "Is the Server Running"

              Http.BadStatus response ->
                (toString response.status)

              Http.BadPayload message _ ->
                "Decoding Failed: " ++ message
              _ ->
                (toString error)
        in
          ( { model | alertMessage = Just errorMessage }, Cmd.none)
      Mark id ->
        let
          markEntry e =
            if e.id == id then
              { e | marked = (not e.marked)}
            else
              e
        in
          ( { model | entries = List.map markEntry model.entries }, Cmd.none )
      CloseAlert ->
        ({ model | alertMessage = Nothing}, Cmd.none)



-- DECODERS/ENCODERS
entryListDecoder : Decoder (List Entry)
entryListDecoder =
  Decode.list entryDecoder

entryDecoder  :  Decoder Entry
entryDecoder =
  Decode.map4 Entry
      (field "id" Decode.int)
      (field "phrase" Decode.string)
      (field "points" Decode.int)
      (succeed False)

scoreDecoder  :  Decoder Score
scoreDecoder =
  Decode.map3 Score
      (field "id" Decode.int)
      (field "name" Decode.string)
      (field "score" Decode.int)

encodeScore : Model -> Encode.Value
encodeScore model =
  Encode.object
      [ ("name", Encode.string model.name)
      , ("score", Encode.int (sumMarkedPoints model.entries))
      ]

-- COMMANDS

generateRandomNumber : Cmd Msg
generateRandomNumber =
    Random.generate NewRandom (Random.int 1 100)

apiUrlPrefix : String
apiUrlPrefix =
  "http://localhost:3000/"

entriesUrl : String
entriesUrl =
    apiUrlPrefix ++ "random-entries"

getEntries : Cmd Msg
getEntries =
    entryListDecoder
        |> Http.get entriesUrl
        |> Http.send NewEntries



postScore : Model -> Cmd Msg
postScore model =
  let
    url =
      apiUrlPrefix ++ "/scores"

    body =
      encodeScore model
          |> Http.jsonBody



    request =
      Http.post url body scoreDecoder
  in
    Http.send NewScore request

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
    [ viewHeader "Bingo"
    , viewPlayer model.name model.gameNumber
    , viewAlertMessage model.alertMessage
    , viewEntryList  model.entries
    , viewScore (sumMarkedPoints model.entries)
    , div [ class "button-group" ]
          [ button [ onClick NewGame ] [ text "New Game"]
          , button [ onClick ShareScore ] [ text "Share Score" ]
          ]
    , viewFooter
    ]

viewAlertMessage : Maybe String -> Html Msg
viewAlertMessage alertMessage =
  case alertMessage of
    Just message ->
        div [ class "alert"]
            [ span [ class "close", onClick CloseAlert ] [text "X"]
            ,text message]
    Nothing ->
        text ""

-- main : Html Msg
-- main =
--    view initialModel
main : Program Never Model Msg
main =
  Html.program
    { init =  ( initialModel,  getEntries )
    , view = view
    , update = update
    , subscriptions = (\_ -> Sub.none)
    }
