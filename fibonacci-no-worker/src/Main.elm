module Main exposing (main)

import Browser
import Html exposing (Html, br, div, img, input, label, text, textarea)
import Html.Attributes exposing (cols, for, height, name, readonly, rows, src, type_, value, width)
import Html.Events exposing (onInput)
import Natural exposing (Natural, one, zero)
import Resource exposing (spinnerGifBase64)
import Task
import Time


type alias TimedValue a =
    { value : a
    , durationMillis : Int
    }


type alias Model =
    { number : Int
    , fibonacci : Maybe (TimedValue Natural)
    }


type Msg
    = NewNumber String
    | NewFibonacci (TimedValue Natural)


init : () -> ( Model, Cmd msg )
init _ =
    ( { number = 0
      , fibonacci = Just { value = zero, durationMillis = 0 }
      }
    , Cmd.none
    )


fibonacci : Int -> Natural
fibonacci n =
    let
        fibonacciTail : Int -> Natural -> Natural -> Natural
        fibonacciTail n2 lastFib fib =
            case n2 of
                0 ->
                    lastFib

                1 ->
                    fib

                _ ->
                    fibonacciTail (n2 - 1)
                        fib
                        (Natural.add lastFib fib)
    in
    fibonacciTail n zero one


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewNumber text ->
            let
                numberUnvalidated : Int
                numberUnvalidated =
                    String.toInt text |> Maybe.withDefault 0

                number : Int
                number =
                    if numberUnvalidated < 0 then
                        0

                    else
                        numberUnvalidated
            in
            ( { model
                | number = number
                , fibonacci = Nothing
              }
            , Task.perform NewFibonacci <|
                Task.map3
                    (\startPosix fib stopPosix ->
                        { value = fib
                        , durationMillis = Time.posixToMillis stopPosix - Time.posixToMillis startPosix
                        }
                    )
                    Time.now
                    (Task.succeed number |> Task.map fibonacci)
                    Time.now
            )

        NewFibonacci value ->
            ( { model | fibonacci = Just value }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    div []
        [ div []
            [ label [ for "input" ] [ text "Input: " ]
            , input [ name "input", type_ "number", value <| String.fromInt model.number, onInput NewNumber ] []
            ]
        , div []
            [ label [ for "output" ] [ text "Fibonacci: " ]
            , case model.fibonacci of
                Just { value, durationMillis } ->
                    div []
                        [ textarea [ name "output", cols 80, rows 24, readonly True ] [ text <| Natural.toString value ]
                        , br [] []
                        , text <| " (took: " ++ String.fromInt durationMillis ++ "ms)"
                        ]

                Nothing ->
                    img [ src <| "data:image/gif;base64," ++ spinnerGifBase64, width 12, height 12 ] []
            ]
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        }
