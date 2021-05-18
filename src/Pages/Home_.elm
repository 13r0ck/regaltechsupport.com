module Pages.Home_ exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Home_ exposing (Params)
import Page
import Palette exposing (FontSize(..), blue500, fontSize, green300, muted, purple600, white)
import Request
import Shared exposing (Temp)
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view shared.temp
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Temp -> Model -> View Msg
view temp model =
    { title = "Regal Tech Support"
    , attributes = []
    , element =
        column [ width fill ]
            [ jumbotron temp
            , column [ height (px 1000) ] []
            ]
    }



-- Helper Functions


jumbotron : Temp -> Element Msg
jumbotron temp =
    let
        w =
            temp.width

        h =
            temp.height + 2

        scaleByHeight =
            (1.5 * toFloat h |> ceiling) > w

        device =
            temp.device.class
    in
    el
        [ width fill
        , height (px h)
        , clip
        , inFront
            (column [ centerX, height fill ]
                [ column [ height fill, Font.color muted, spacing 20 ]
                    [ image [ width (px 200), height (px 150), centerX ] { src = "/img/logo.svg", description = "logo" }
                    , paragraph
                        [ Font.center
                        , Font.family [ Font.typeface "regalBold", Font.sansSerif ]
                        , fontSize device Xlg
                        ]
                        [ text "Regal Tech Support" ]
                    , paragraph [ Font.center, fontSize device Md, Font.family [ Font.typeface "regal", Font.sansSerif ] ] [ text "IT Support at home and at work in Colorado Springs." ]
                    , Input.button [ centerX ] { onPress = Nothing, label = el [ paddingXY 40 15, Font.family [ Font.typeface "regalBold", Font.sansSerif ], fontSize device Md, Font.color white, Border.rounded 10, Background.gradient { angle = degrees 45, steps = [ green300, blue500, purple600 ] } ] (text "Contact Us") }
                    ]
                , el [ height (fillPortion 4) ] none
                ]
            )
        ]
        (image
            [ if scaleByHeight then
                width (px ((1.5 * toFloat h) |> ceiling))

              else
                width (px w)
            , if scaleByHeight then
                moveUp 0

              else
                moveUp ((((2.0 / 3.0) * toFloat w) - toFloat h) / 3)
            , alignBottom
            , centerX
            ]
            { src = "/img/jumbotron.svg", description = "A fantasy castle in the mountains" }
        )
