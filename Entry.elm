module Entry exposing(..)

import Html exposing(..)
import Html.Attributes exposing(..)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)

type alias Entry =
  { id : Int
  , phrase : String
  , points : Int
  , marked :  Bool
  }

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

markEntryWithId : List Entry -> Int -> List Entry
markEntryWithId entries id =
  let
    markEntry e =
      if e.id == id then
        { e | marked = (not e.marked)}
      else
        e
  in
    List.map markEntry entries

getEntries : (Result Http.Error (List Entry) ->  msg ) -> String -> Cmd msg
getEntries msg entriesUrl =
    entryListDecoder
        |> Http.get entriesUrl
        |> Http.send msg

viewEntryItem : (Int -> msg) -> Entry -> Html msg
viewEntryItem msg item =
  li [  classList [ ("marked", item.marked) ], onClick (msg item.id) ]
    [ span [ class "phrase" ][ text item.phrase ]
    , span [ class "points" ][ text (toString item.points)]
    ]

viewEntryList : (Int -> msg) -> List Entry -> Html msg
viewEntryList msg entries =
  let
    entryItems =
     List.map (viewEntryItem msg) entries
  in
    ul [] entryItems

sumMarkedPoints : List Entry -> Int
sumMarkedPoints entries =
      entries
      |> List.filter .marked
      |> List.foldl(\entry sum -> sum + entry.points) 0
