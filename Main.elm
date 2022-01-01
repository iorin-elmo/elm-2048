module Main exposing (..)
import Browser
import Browser.Events exposing (onKeyDown)
import Html exposing (Html, div)
import Svg exposing (Svg, svg)
import Svg.Attributes as SAttr
import Random exposing (Generator, generate)
import Task
import ListWrapper.Dict as Dict exposing (Dict)
import ListWrapper.Set as Set exposing (Set)
import Json.Decode as Decoder exposing (Decoder, string)

type alias Model =
  { field : Field
  , leftSpace : Set (Int, Int)
  , isOver : Bool
  }

type alias Field = Dict (Int, Int) Int


type Msg
  = Pressed String
  | GetNext ((Int,Int),Int)

init : Model
init =
  { field = Dict.empty
  , leftSpace =
      List.range 0 15
        |> List.map (\n -> (modBy 4 n, n//4))
        |> Set.fromList
  , isOver = False
  }

type Direction
  = Left
  | Right
  | Up
  | Down

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Pressed str -> 
      case str of
        "ArrowLeft" ->
          move Left model.field
            |> updateField model
        "ArrowRight" ->
          move Right model.field
            |> updateField model
        "ArrowUp" ->
          move Up model.field
            |> updateField model
        "ArrowDown" ->
          move Down model.field
            |> updateField model

        _ -> (model, Cmd.none)

    GetNext ((x,y),n) ->
      case Dict.get (x,y) model.field of
        Just _ ->
          ( model
          , Cmd.none
          )
        _ ->
          ( { model | field = Dict.insert (x,y) n model.field }
          , Cmd.none
          )

move : Direction -> Field -> Field
move direction dict =
  case direction of
    Down -> moveDown dict
    Up -> moveUp dict
    Right -> moveRight dict
    Left -> moveLeft dict



moveUp : Field -> Field
moveUp dict =
  let
    helper (x,y) d =
      if y == 0
      then d
      else
        case (Dict.get (x,y) d, Dict.get (x,y-1) d) of
          (Nothing, _) -> d
          (Just n, Nothing) ->
            Dict.remove (x,y) d
              |> Dict.insert (x,y-1) n
              |> helper (x,y-1)

          (Just n, Just m) ->
            if n /= m
            then d
            else
              Dict.remove (x,y) d
                |> Dict.insert (x,y-1) (n+1)

    move1row row dict_ =
      List.range 1 3
        |> List.map (\y -> (row, y))
        |> List.foldl
            helper
            dict_

  in
    List.foldl
      (\n d ->
        move1row n d
      )
      dict
      (List.range 0 3)

moveDown : Field -> Field
moveDown dict =
  let
    helper (x,y) d =
      if y == 3
      then d
      else
        case (Dict.get (x,y) d, Dict.get (x,y+1) d) of
          (Nothing, _) -> d
          (Just n, Nothing) ->
            Dict.remove (x,y) d
              |> Dict.insert (x,y+1) n
              |> helper (x,y+1)

          (Just n, Just m) ->
            if n /= m
            then d
            else
              Dict.remove (x,y) d
                |> Dict.insert (x,y+1) (n+1)

    move1row row dict_ =
      List.range 0 2
        |> List.reverse
        |> List.map (\y -> (row, y))
        |> List.foldl
            helper
            dict_

  in
    List.foldl
      (\n d ->
        move1row n d
      )
      dict
      (List.range 0 3)

moveLeft : Field -> Field
moveLeft dict =
  let
    helper (x,y) d =
      if x == 0
      then d
      else
        case (Dict.get (x,y) d, Dict.get (x-1,y) d) of
          (Nothing, _) -> d
          (Just n, Nothing) ->
            Dict.remove (x,y) d
              |> Dict.insert (x-1,y) n
              |> helper (x-1,y)

          (Just n, Just m) ->
            if n /= m
            then d
            else
              Dict.remove (x,y) d
                |> Dict.insert (x-1,y) (n+1)

    move1col col dict_ =
      List.range 1 3
        |> List.map (\x -> (x, col))
        |> List.foldl
            helper
            dict_

  in
    List.foldl
      (\n d ->
        move1col n d
      )
      dict
      (List.range 0 3)

moveRight : Field -> Field
moveRight dict =
  let
    helper (x,y) d =
      if x == 3
      then d
      else
        case (Dict.get (x,y) d, Dict.get (x+1,y) d) of
          (Nothing, _) -> d
          (Just n, Nothing) ->
            Dict.remove (x,y) d
              |> Dict.insert (x+1,y) n
              |> helper (x+1,y)

          (Just n, Just m) ->
            if n /= m
            then d
            else
              Dict.remove (x,y) d
                |> Dict.insert (x+1,y) (n+1)

    move1col col dict_ =
      List.range 0 2
        |> List.reverse
        |> List.map (\x -> (x, col))
        |> List.foldl
            helper
            dict_

  in
    List.foldl
      (\n d ->
        move1col n d
      )
      dict
      (List.range 0 3)

updateField model newDict =
  if model.field == newDict
  then
    (model, Cmd.none)
  else
    ( { model | field = newDict }
    , rollNext (findSpace newDict)
        |> generate GetNext
    )

findSpace dict =
  let
    occupiedList =
      dict
        |> Dict.toList
        |> List.unzip
        |> Tuple.first
  in
    List.range 0 15
      |> List.map (\n -> (modBy 4 n, n//4))
      |> List.filter
          (\(x,y) ->
              List.all ((/=) (x,y)) occupiedList
          )
      |> Set.fromList

rollNext : Set (Int,Int) -> Generator ((Int,Int),Int)
rollNext set =
  (Random.weighted (75,0) [(25,1)])
    |> Random.pair
        ( set
            |> Set.toList
            |> getHeadTailFromList
            |> Maybe.withDefault ((-1,0),[])
            |> (\(hd,tl) -> Random.uniform hd tl)
        )

getHeadTailFromList li =
  case li of
    (hd::tl) ->
      Just (hd, tl)
    _ -> Nothing

i2s = String.fromInt

n2color n =
  case modBy 10 n of
    0 -> "white"
    1 -> "lightyellow"
    2 -> "lightsalmon"
    3 -> "orange"
    4 -> "orangered"
    5 -> "red"
    6 -> "yellow"
    7 -> "gold"
    8 -> "goldenrod"
    9 -> "darkorange"
    _ -> "?"

view model =
  List.range 0 15
    |> List.map (\n -> (modBy 4 n, n//4))
    |> List.concatMap
        (\(x,y) ->
          case Dict.get (x,y) model.field of
            Just n ->
              [ Svg.rect
                [ SAttr.x <| i2s (x*50)
                , SAttr.y <| i2s (y*50)
                , SAttr.width "50"
                , SAttr.height "50"
                , SAttr.fill (n2color n)
                , SAttr.fillOpacity "20"
                ][]
              , Svg.text_
                  [ SAttr.x <| i2s (x*50)
                  , SAttr.y <| i2s (y*50+30)
                  ]
                  [ Svg.text
                      ( (String.pad 4 '　' << i2s) (2^(n+1)))
                  ]
              ]
            _ ->
              [ Svg.rect
                [ SAttr.x <| i2s (x*50)
                , SAttr.y <| i2s (y*50)
                , SAttr.width "50"
                , SAttr.height "50"
                , SAttr.fill "gray"
                , SAttr.fillOpacity "20"
                ][]
              ]
              
        )
    |> svg
        [ SAttr.width "200"
        , SAttr.height "200"
        , SAttr.viewBox "0 0 200 200"
        ]


subscriptions _ =
  onKeyDown keyDecoder

keyDecoder : Decoder Msg
keyDecoder =
  (Decoder.field "key" string)
    |> Decoder.map Pressed

main : Program () Model Msg
main =
  Browser.element
    { init = \_ ->
        ( init
        , Random.pair
            ( Random.pair
                ( Random.int 0 3 )
                ( Random.int 0 3 )
            )
            ( Random.weighted (75,0) [(25,1)] )
              |> generate GetNext
        )
    , view = view
    , update = update
    , subscriptions = subscriptions
    }

