module Shared exposing
    ( Flags
    , Model
    , Msg
    , Temp
    , init
    , subscriptions
    , update
    )

import Browser.Events
import Element exposing (..)
import Http
import Json.Decode as Json
import Request exposing (Request)


type alias Flags =
    { width : Int
    , height : Int
    }


type alias Model =
    { temp : Temp
    }


type alias Temp =
    { width : Int
    , height : Int
    , device : Device
    }


type Msg
    = WindowResized Int Int


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( { temp =
            { width = flags.width
            , height = flags.height
            , device = classifyDevice { width = flags.width, height = flags.height }
            }
      }
    , Cmd.none
    )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        WindowResized w h ->
            ( { model | temp = model.temp |> (\t -> { t | device = classifyDevice { width = w, height = h }, width = w, height = h }) }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Browser.Events.onResize WindowResized



-- Helper Functions


classifyDevice : { window | height : Int, width : Int } -> Device
classifyDevice window =
    -- Tested in this ellie:
    -- https://ellie-app.com/68QM7wLW8b9a1
    { class =
        let
            width =
                window.width

            longSide =
                max window.width window.height

            shortSide =
                min window.width window.height
        in
        if width <= 600 then
            Phone

        else if width > 600 && width <= 1200 then
            Tablet

        else if width > 1200 && width <= 1920 then
            Desktop

        else
            BigDesktop
    , orientation =
        if window.width < window.height then
            Portrait

        else
            Landscape
    }
