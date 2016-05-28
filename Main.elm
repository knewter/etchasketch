import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Keyboard.Extra
import Time exposing (Time, second)


type alias Model =
  { points : List Point
  , x : Int
  , y : Int
  , keyboardModel : Keyboard.Extra.Model
  }


type alias Point = (Int, Int)


type Msg
  = KeyboardExtraMsg Keyboard.Extra.Msg
  | Tick Time
  | Shake


init : ( Model, Cmd Msg )
init =
  let
    ( keyboardModel, keyboardCmd ) = Keyboard.Extra.init
  in
    ( { points = [(0, 0)]
      , x = 0
      , y = 0
      , keyboardModel = keyboardModel
      }
    , Cmd.batch
      [ Cmd.map KeyboardExtraMsg keyboardCmd
      ]
    )


shakeButton : Html Msg
shakeButton =
  Html.button [onClick Shake] [ Html.text "Shake it good" ]


view : Model -> Html Msg
view model =
  div []
    [ collage 800 800
        [ (drawLine model.points) ]
        |> Element.toHtml
    , shakeButton
    ]


drawLine : List Point -> Form
drawLine points =
  let
    -- Our points are integers, but a path needs a list of floats.  We'll make a
    -- function to turn a 2-tuple of ints into a 2-tuple of floats
    intsToFloats : (Int, Int) -> (Float, Float)
    intsToFloats (x, y) =
      (toFloat x, toFloat y)

    -- Then we'll map our points across that function
    shape = path (List.map intsToFloats points)
  in
    -- Finally, we'll trace that list of points in solid red
    shape
    |> traced (solid red)


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    KeyboardExtraMsg keyMsg ->
      let
        ( keyboardModel, keyboardCmd ) =
            Keyboard.Extra.update keyMsg model.keyboardModel
      in
        ( { model | keyboardModel = keyboardModel }
        , Cmd.map KeyboardExtraMsg keyboardCmd
        )

    Tick _ ->
      let
        {x, y} = Keyboard.Extra.arrows model.keyboardModel
        newX = model.x + x
        newY = model.y + y
      in
        case (x, y) of
          (0, 0) ->
            model ! []
          _ ->
            { model | points = (newX, newY) :: model.points, x = newX, y = newY } ! []

    Shake ->
      { model | points = [] } ! []


main : Program Never
main =
  App.program
    { init = init
    , update = update
    , view = view
    , subscriptions = subscriptions
    }


subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.batch
    [ Sub.map KeyboardExtraMsg Keyboard.Extra.subscriptions
    , Time.every (1/30 * second) Tick
    ]
