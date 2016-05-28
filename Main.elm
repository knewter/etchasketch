import Color exposing (..)
import Collage exposing (..)
import Element exposing (..)
import Keyboard
import Html exposing (..)
import Html.Events exposing (onClick)
import Html.App as App
import Keyboard.Extra
import Time exposing (Time, second)
import AnimationFrame
import Animation exposing (..)


type alias Model =
  { points : List Point
  , x : Int
  , y : Int
  , keyboardModel : Keyboard.Extra.Model
  , clock : Time
  , animation : Animation
  , animations : List (Time -> Animation)
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
      , clock = 0
      , animation = static 0
      , animations = []
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
  let
    angle =
      animate model.clock model.animation
  in
    div []
      [ collage 800 800
          [ (rotate (degrees angle) (drawLine model.points)) ]
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


shakeAnimation : Time -> Animation
shakeAnimation t =
  animation t
  |> from 0
  |> to 40
  |> duration (500*Time.millisecond)

shakeAnimation' : Time -> Animation
shakeAnimation' t =
  animation t
  |> from 40
  |> to -20
  |> duration (500*Time.millisecond)

shakeAnimation'' : Time -> Animation
shakeAnimation'' t =
  animation t
  |> from -20
  |> to 10
  |> duration (500*Time.millisecond)

shakeAnimation''' : Time -> Animation
shakeAnimation''' t =
  animation t
  |> from 10
  |> to 0
  |> duration (500*Time.millisecond)


animations : List (Time -> Animation)
animations =
  [ shakeAnimation
  , shakeAnimation'
  , shakeAnimation''
  , shakeAnimation'''
  ]


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

    Tick dt ->
      let
        {x, y} = Keyboard.Extra.arrows model.keyboardModel
        newX = model.x + x
        newY = model.y + y
        -- We need to also return our list of new animations in our 3-tuple now
        (newPoints, newAnimation, newAnimations) =
          -- We'll start off checking to see if the current animation is done, now
          case (isDone model.clock model.animation) of
            True ->
              -- If it's done, we'll set up the next animation and reduce the
              -- list by one element.  If we're out of animations, we'll move to
              -- the static animation and an empty list of animations
              let
                nextAnimation =
                  case List.head model.animations of
                    Just animation -> animation model.clock
                    Nothing -> static 0
                nextAnimations = (List.tail model.animations) |> Maybe.withDefault([])
                -- We know the shaking just finished if the current animation is
                -- done, it's not static, and the nextAnimation is static
                justFinished =
                  nextAnimation `equals` (static 0) &&
                  not (model.animation `equals` (static 0))
                -- If we just finished our shake, we'll clear out the points
                nextPoints =
                  case justFinished of
                    True -> []
                    False -> model.points
              in
                (nextPoints, nextAnimation, nextAnimations)
            False ->
              -- If the animation is not done, we won't change anything
              (model.points, model.animation, model.animations)
        newPoints' =
          case (x, y) of
            (0, 0) ->
              newPoints
            _ ->
              (newX, newY) :: newPoints
        model' =
          { model
          | points = (newX, newY) :: newPoints'
          , clock = model.clock + dt
          , animation = newAnimation
          -- And we'll update our list of animations
          , animations = newAnimations
          }
      in
        case (x, y) of
          (0, 0) ->
            model' ! []
          _ ->
            { model'
            | x = newX
            , y = newY
            } ! []

    Shake ->
      { model
      | animations = animations
      } ! []


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
    , AnimationFrame.diffs Tick
    ]
