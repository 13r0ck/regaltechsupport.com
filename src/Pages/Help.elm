module Pages.Help exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Events as Events
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (OptionState, newPassword)
import Element.Region as Region
import File.Download as Download
import Gen.Params.Help exposing (Params)
import Html exposing (br)
import Html.Attributes exposing (accept, class, id)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (hardcoded, optional, required)
import Json.Encode as Encode
import Page
import Palette exposing (FontSize(..), black, blue500, fontSize, green300, green500, green700, maxWidth, muted, purple600, regal, regalBold, warning, white)
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
    { code : String }


init : ( Model, Cmd Msg )
init =
    ( { code = "" }, Cmd.none )



-- UPDATE


type Msg
    = CodeChanged String


type alias VerifyResponseHeader =
    { access_control_allow_credentials : Bool
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CodeChanged s ->
            let
                clean str =
                    str |> String.trim |> String.replace "-" "" |> String.left 11
            in
            if s |> String.toList |> List.all (\c -> List.any (\char -> c == char) [ '0', '1', '2', '3', '4', '5', '6', '7', '8', '9', '-' ]) then
                ( { model
                    | code =
                        if String.length s < String.length model.code then
                            s

                        else
                            s |> String.trim |> String.replace "-" "" |> addDashes "" |> String.left 11
                  }
                , if String.length s == 11 then
                    {- This responds with the html data for the page.
                        I Should be able to use this response to validate
                        if the code is
                        1. valid or not and then notify on regaltechsupport.com rather than error on the redirect
                        2. Even download the app if the code is valid.
                       Http.get { url = ("https://www.remotepc.com/rpchd/setupVerifyDownload?id=" ++ (clean s))
                                , expect = Http.expectString VerifyResponse
                                }
                    -}
                    Nav.load ("https://www.remotepc.com/rpchd/setupVerifyDownload?id=" ++ clean s)

                  else
                    Cmd.none
                )

            else
                ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Temp -> Model -> View Msg
view temp model =
    let
        w =
            temp.width

        h =
            temp.height

        device =
            temp.device.class

        isPhone =
            device == Phone
    in
    { title = "Regal Tech Support"
    , attributes =
        [ inFront (contactUs temp model)
        , inFront (navbar temp)

        --, inFront (submitalert temp model)
        ]
    , element =
        column [ width fill, height (px h), clip ]
            [ jumbotron temp
            ]
    }



-- Helper Functions


addDashes : String -> String -> String
addDashes acc s =
    if String.length (String.left 3 s) < 3 then
        acc ++ String.left 3 s

    else
        addDashes (acc ++ String.left 3 s ++ "-") (String.slice 3 (String.length s) s)


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
            { src = "/img/jumbotron.svg", description = "A castle in the mountains" }
        )


contactUs : Temp -> Model -> Element Msg
contactUs temp model =
    let
        device =
            temp.device.class

        w =
            temp.width

        h =
            temp.height

        isPhone =
            device == Phone

        isBigDesktop =
            device == BigDesktop

        isDesktop =
            device == Desktop

        eachHeight =
            px 50

        contactInfo t =
            el [ height (px 50), padding 5, Border.rounded 10, Font.color white, regalBold, Background.color purple600, width fill ] (el [ centerY ] (text t))
    in
    el
        [ width fill
        , height fill
        , behindContent
            (el
                [ width fill
                , height fill
                , Background.gradient
                    { angle = degrees 165
                    , steps = [ rgba255 87 83 78 0.7, rgba255 17 24 39 0.9 ]
                    }
                ]
                none
            )
        ]
        (column
            [ Background.color white
            , width (px (min 600 w))
            , centerX
            , centerY
            , padding 25
            , spacing 25
            , Border.shadow { blur = 20, color = rgb 0.25 0.25 0.3, offset = ( 0, 0 ), size = 1 }
            , rounded 25
            , clip
            ]
            [ row [ spacing 25, width fill, fontSize device Sm ]
                [ column [ width fill, spacing 10 ]
                    [ contactInfo "Enter your 9-digit code:"
                    ]
                , column [ width fill, spacing 10 ]
                    [ Input.text
                        [ height eachHeight
                        , Border.width 3
                        , Border.color purple600
                        , Border.rounded 10
                        , Font.center
                        ]
                        { label = Input.labelHidden "9 digit code", onChange = CodeChanged, placeholder = Just (Input.placeholder [] (el [ regal, Font.center, width fill ] (text "123 - 456 - 789"))), text = model.code }
                    ]
                ]
            ]
        )


navbar : Temp -> Element msg
navbar temp =
    let
        device =
            temp.device.class

        isMobile =
            device == Phone || device == Tablet

        navBtn icon t =
            row
                ([ height (px 50)
                 , Border.rounded 500
                 , fontSize device Sm
                 , spacing 10
                 , padding
                    (if isMobile then
                        30

                     else
                        10
                    )
                 , inFront (el [ width (px 50), height (px 50), Border.rounded 100, centerY, Background.gradient { angle = 0, steps = [ blue500, green500 ] }, alignLeft ] (image [ width (px 20), height (px 20), centerX, centerY ] { src = icon, description = "icon" }))
                 ]
                    ++ (if isMobile then
                            []

                        else
                            [ htmlAttribute <| class "blur", padding 10, mouseOver [ Background.color white ] ]
                       )
                )
                (if isMobile then
                    []

                 else
                    [ el [ width (px 50) ] none
                    , el [ regalBold ] (text t)
                    ]
                )
    in
    row [ width fill, Region.navigation ]
        (if isMobile then
            [ link [ alignLeft, padding 10 ] { url = "mailto:hello@regaltechsupport.com", label = navBtn "/img/email.svg" "hello@regaltechsupport.com" }
            , link [ padding 10, centerX ] { url = "/", label = navBtn "/img/home.svg" "Home" }
            , link [ alignRight, padding 10 ] { url = "tel:7194409462", label = navBtn "/img/phone.svg" "(719) 440 - 9462" }
            ]

         else
            [ link [ alignLeft, padding 10 ] { url = "/", label = navBtn "/img/home.svg" "Home" }
            , link [ alignLeft, padding 10 ] { url = "tel:7194409462", label = navBtn "/img/phone.svg" "(719) 440 - 9462" }
            , link [ alignRight, padding 10 ] { url = "mailto:hello@regaltechsupport.com", label = navBtn "/img/email.svg" "hello@regaltechsupport.com" }
            ]
        )
