module Bingo exposing (..)

import Html exposing (..)
import Random exposing(..)
import Html.Attributes exposing (..)
import Html.Events exposing(onClick, onInput)
import Http
import Json.Decode as Decode exposing(Decoder, field, succeed)
import Json.Encode as Encode
import ViewHelpers exposing(..)
import Entry


type GameState = EnteringName | Playing

type alias Model =
  { name  : String
  , gameNumber : Int
  , entries: List Entry.Entry
  , alertMessage: Maybe String
  , nameInput: String
  , gameState: GameState
  }


type alias Score =
  { id : Int
  , name: String
  , score: Int
  }

-- MODEL
initialModel : Model
initialModel =
  { name = "Anon"
  , gameNumber = 1
  , entries = []
  , alertMessage = Nothing
  , nameInput = ""
  , gameState = EnteringName
  }




-- UPDATE

type Msg
    = NewGame
    | Mark Int
    | NewRandom Int
    | NewEntries (Result Http.Error (List Entry.Entry))
    | CloseAlert
    | ShareScore
    | NewScore (Result Http.Error Score)
    | SetNameInput String
    | SaveName
    | CancelName
    | ChangeGameState GameState

update : Msg -> Model -> ( Model , Cmd Msg )
update msg model =
    case msg of
      ChangeGameState state ->
        ({ model | gameState = state }, Cmd.none)
      CancelName ->
        ({ model | nameInput = "", gameState = Playing}, Cmd.none)
      SaveName ->
        ({model | nameInput = "", name = model.nameInput, gameState = Playing}, Cmd.none)
      SetNameInput value ->
        ({ model | nameInput = value }, Cmd.none)
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
          ({model | entries = Entry.markEntryWithId model.entries id}, Cmd.none)
      CloseAlert ->
        ({ model | alertMessage = Nothing}, Cmd.none)



-- DECODERS/ENCODERS
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
      , ("score", Encode.int (Entry.sumMarkedPoints model.entries))
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
    Entry.getEntries NewEntries entriesUrl



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

isScoreZero : Model -> Bool
isScoreZero model =
  (Entry.sumMarkedPoints model.entries) == 0




viewPlayer : String -> Int -> Html Msg
viewPlayer name gameNumber =
    h2 [ id "info", class "classy"]
        [ a [ href "#",  onClick (ChangeGameState EnteringName)]
            [text name]
        , text (" - Game #" ++ (toString gameNumber))
        ]

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
    , viewAlertMessage CloseAlert model.alertMessage
    , viewNameInput model
    , Entry.viewEntryList Mark  model.entries
    , viewScore (Entry.sumMarkedPoints model.entries)
    , div [ class "button-group" ]
          [ primaryButton NewGame "New Game"
          , primaryButton ShareScore "Share Score"
          ]
    , viewFooter
    ]

viewNameInput : Model -> Html Msg
viewNameInput model =
  case model.gameState of
    EnteringName ->
      div [ class "name-input"]
          [ input
              [ type_ "text"
              , placeholder "Who's playing? "
              , autofocus True
              , value model.nameInput
              , onInput SetNameInput
              ]
              []
            , primaryButton SaveName "Save"
            , primaryButton SaveName "Cancel"
          ]
    Playing ->
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
