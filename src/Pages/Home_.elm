module Pages.Home_ exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border exposing (rounded)
import Element.Font as Font exposing (center)
import Element.Input as Input exposing (newPassword)
import Element.Region as Region
import Email
import Gen.Params.Home_ exposing (Params)
import Html exposing (br)
import Html.Attributes exposing (accept, class, id)
import Html.Events
import Http
import Json.Decode as Json
import Json.Encode as Encode
import List exposing (intersperse)
import Page
import Palette exposing (FontSize(..), black, blue500, fontSize, green300, green500, green700, maxWidth, muted, purple600, regal, regalBold, warning, white)
import Ports exposing (copyText, successfulCopy)
import Process
import Random
import Random.Extra
import Request
import Shared exposing (Temp)
import Task
import Time
import Url exposing (Protocol(..))
import View exposing (View, placeholder)


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
    { subText : List SubText
    , checker : String
    , password : Maybe String
    , validEmail : Email
    , wordList : Maybe (List String)
    , wordsLen : Maybe Int
    , alertCopy : Alert
    , alertSubmit : Alert
    }


type Alert
    = Good
    | Bad
    | DisplayHidden


type alias PassCode =
    { first : String
    , second : String
    , third : String
    , specialChar : String
    , num : Int
    , numIndex : Int
    , capsIndex : Int
    }


type alias SubText =
    { title : String
    , image : String
    , text : String
    }


type alias FormResponse =
    { next : String
    , ok : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { subText =
            [ SubText "What Makes Us Different?" "/img/jester_weld.svg" "Typical IT support is there only when things break. We prefer a proactive approach. By spending time to understand your situation we will create a reliable technology experience for you! We believe that reliability and security are possible with any budget!"
            , SubText "What We Do." "/img/jester_bluescreen.svg" "We create reliable IT ecosystems. We build websites, custom software, as well as design and maintain your IT infrastructure to keep your buisness functioning. You to can run a Tech company, we can help."
            , SubText "24/7 Support" "/img/jester_fire.svg" "Nobody gets to choose when things break. We will be there for you whenever and wherever you need us!"
            ]
      , checker = ""
      , password = Nothing
      , validEmail = Invalid
      , wordList = Nothing
      , wordsLen = Nothing
      , alertCopy = DisplayHidden
      , alertSubmit = DisplayHidden
      }
    , Http.get { url = "/misc/words.txt", expect = Http.expectString GotWords }
    )



-- UPDATE


type Msg
    = CheckerChanged String
    | GotWords (Result Http.Error String)
    | GenPass
    | PassGened PassCode
    | CopyPass
    | AlertCopy Bool
    | ClearCopy ()
    | ClearSubmit ()
    | SubmitChecker
    | CheckerSubmited (Result Http.Error FormResponse)


type Email
    = Valid
    | Invalid
    | Error


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CheckerChanged s ->
            if model.validEmail == Error then
                ( { model
                    | checker = s
                    , validEmail =
                        if Email.isValid s then
                            Valid

                        else
                            Error
                  }
                , Cmd.none
                )

            else
                ( { model | checker = s }, Cmd.none )

        GotWords result ->
            case result of
                Ok fullText ->
                    let
                        words =
                            List.take 3000 (List.drop 32 (String.split "\n" fullText))
                    in
                    ( { model | wordList = Just words, wordsLen = Just (List.length words) }, Cmd.none )

                Err _ ->
                    ( model, Cmd.none )

        GenPass ->
            ( model
            , case model.wordList of
                Just words ->
                    case model.wordsLen of
                        Just wordsLen ->
                            Random.generate
                                PassGened
                                (Random.map PassCode (Random.uniform "a" words)
                                    |> Random.Extra.andMap (Random.uniform "b" words)
                                    |> Random.Extra.andMap (Random.uniform "c" words)
                                    |> Random.Extra.andMap (Random.uniform "!" [ "!", "\"", "$", "^", "&", "*", "(", ")" ])
                                    |> Random.Extra.andMap (Random.int 0 10)
                                    |> Random.Extra.andMap (Random.int 0 2)
                                    |> Random.Extra.andMap (Random.int 0 2)
                                )

                        Nothing ->
                            Cmd.none

                Nothing ->
                    Cmd.none
            )

        PassGened p ->
            ( { model
                | password =
                    [ p.first, p.second, p.third ]
                        |> List.indexedMap
                            (\i v ->
                                if i == p.capsIndex then
                                    String.toUpper v

                                else
                                    v
                            )
                        |> List.indexedMap
                            (\i v ->
                                if i == p.numIndex then
                                    [ String.fromInt p.num, v ]

                                else
                                    [ v ]
                            )
                        |> List.map (\l -> List.foldl (++) "" l)
                        |> List.intersperse p.specialChar
                        |> List.foldl (++) ""
                        |> Just
              }
            , Cmd.none
            )

        CopyPass ->
            ( model
            , case model.password of
                Just pass ->
                    copyText pass

                Nothing ->
                    Cmd.none
            )

        AlertCopy b ->
            ( { model
                | alertCopy =
                    if b then
                        Good

                    else
                        Bad
              }
            , Task.perform ClearCopy (Process.sleep 2000)
            )

        ClearCopy _ ->
            ( { model | alertCopy = DisplayHidden }, Cmd.none )

        ClearSubmit _ ->
            ( { model | alertSubmit = DisplayHidden }, Cmd.none )

        SubmitChecker ->
            if Email.isValid model.checker then
                ( model, Http.post { url = "https://formspree.io/f/xrgoadlk", body = Http.jsonBody <| Encode.object [ ( "name", Encode.string model.checker ) ], expect = Http.expectJson CheckerSubmited (Json.map2 FormResponse (Json.field "next" Json.string) (Json.field "ok" Json.bool)) } )

            else
                ( { model | validEmail = Error }, Cmd.none )

        CheckerSubmited response ->
            ( { model
                | alertSubmit =
                    case response of
                        Ok json ->
                            if json.ok then
                                Good

                            else
                                Bad

                        error ->
                            Bad
              }
            , Task.perform ClearSubmit (Process.sleep 5000)
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    successfulCopy (\c -> AlertCopy c)



-- VIEW


view : Temp -> Model -> View Msg
view temp model =
    let
        w =
            temp.width

        device =
            temp.device.class

        isPhone =
            device == Phone
    in
    { title = "Regal Tech Support"
    , attributes = [ inFront (navbar temp), inFront (copyalert temp model.alertCopy), inFront (submitalert temp model) ]
    , element =
        column [ width fill ]
            [ jumbotron temp
            , column
                [ if isPhone then
                    width fill

                  else
                    width (px (toFloat w * 0.8 |> round) |> maximum maxWidth)
                , centerX
                , spacing 50
                ]
                [ column []
                    [ subTexts temp model.subText
                    , Input.button [ centerX ] { onPress = Nothing, label = el [ paddingXY 40 15, regalBold, fontSize device Md, Font.color white, Border.rounded 10, htmlAttribute <| class "floatingBtn", Background.gradient { angle = degrees 45, steps = [ green300, blue500, purple600 ] } ] (text "Contact Us") }
                    ]
                , tools temp model
                ]
            ]
    }



-- Helper Functions


copyalert : Temp -> Alert -> Element Msg
copyalert temp state =
    let
        device =
            temp.device.class
    in
    case state of
        Good ->
            el [ htmlAttribute <| class "slide_up_and_out", width fill, paddingXY 0 25, alignBottom, regalBold, Font.color white, fontSize device Lg, Font.center, Background.color green500 ] (text "Copied!")

        Bad ->
            el [ htmlAttribute <| class "slide_up_and_out", width fill, paddingXY 0 25, alignBottom, regalBold, Font.color white, fontSize device Lg, Font.center, Background.color warning ] (text "Copy Failed.")

        DisplayHidden ->
            Element.none


submitalert : Temp -> Model -> Element Msg
submitalert temp model =
    let
        state =
            model.alertSubmit

        device =
            temp.device.class
    in
    case state of
        Good ->
            paragraph [ htmlAttribute <| class "slide_up_and_out_long", width fill, paddingXY 0 25, alignBottom, regalBold, Font.color white, fontSize device Md, Font.center, Background.color green500 ] [ text "Success!", html <| br [] [], text "We will contact you at ", text model.checker, text " soon!" ]

        Bad ->
            paragraph [ htmlAttribute <| class "slide_up_and_out_long", width fill, paddingXY 0 25, alignBottom, regalBold, Font.color white, fontSize device Md, Font.center, Background.color warning ] [ text "Submission Failed.", html <| br [] [], text "Contact us at hello@regaltechsupport.com and we will manualy run the scan." ]

        DisplayHidden ->
            Element.none


navbar : Temp -> Element Msg
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
        [ link [ alignLeft, padding 10 ] { url = "mailto:hello@regaltechsupport.com", label = navBtn "/img/email.svg" "hello@regaltechsupport.com" }
        , link [ alignRight, padding 10 ] { url = "tel:7194409462", label = navBtn "/img/phone.svg" "(719) 440 - 9462" }
        ]


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
                        , regalBold
                        , Region.heading 1
                        , fontSize device Xlg
                        ]
                        [ text "Regal Tech Support" ]
                    , paragraph [ Font.center, fontSize device Md, Region.heading 2, regal ] [ text "IT Support at home and at work in Colorado Springs." ]
                    , Input.button [ centerX ] { onPress = Nothing, label = el [ paddingXY 40 15, regalBold, fontSize device Md, Font.color white, Border.rounded 10, htmlAttribute <| class "floatingBtn", Background.gradient { angle = degrees 45, steps = [ green300, blue500, purple600 ] } ] (text "Contact Us") }
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
            { src = "/img/jumbotron.svg", description = "A castle in the mountains" }
        )


subTexts : Temp -> List SubText -> Element msg
subTexts temp texts =
    let
        device =
            temp.device.class

        isMobile =
            device == Phone || device == Tablet

        spacer i =
            el [ width fill, height (px 20), Border.rounded 100, clip, behindContent (image [ width fill, moveUp (toFloat (i * 80)) ] { src = "/img/polygons-gray.svg", description = "spacer" }) ] none

        intersperceSpacer len i item =
            if not (i == len - 1) then
                [ item, spacer i ]

            else
                [ item ]

        sub i t =
            let
                img =
                    image
                        [ width
                            (if isMobile then
                                fill |> maximum 300

                             else
                                fill
                            )
                        , centerX
                        ]
                        { src = t.image, description = "Company mascot" }

                content =
                    paragraph
                        [ width
                            (if isMobile then
                                fill

                             else
                                fillPortion 2
                            )
                        , fontSize device Md
                        ]
                        [ text t.text ]
            in
            column [ padding 50, width (fill |> maximum maxWidth) ]
                [ paragraph [ fontSize device Lg, regalBold, Region.heading 3 ] [ text t.title ]
                , (if isMobile then
                    column

                   else
                    row
                  )
                    [ width fill, spacing 50 ]
                    (if isMobile || (modBy 2 i == 0) then
                        [ img, content ]

                     else
                        [ content, img ]
                    )
                ]
    in
    column [ spacing 50 ]
        (List.concat (List.indexedMap (intersperceSpacer (List.length texts)) (List.indexedMap sub texts)))


tools : Temp -> Model -> Element Msg
tools temp model =
    let
        device =
            temp.device.class

        isMobile =
            isPhone || device == Tablet

        isPhone =
            device == Phone

        password =
            let
                gray =
                    [ Background.color (rgb 0.8 0.8 0.95), Border.rounded 2, padding 3 ]
            in
            column [ width fill, height fill, Background.color white, Border.rounded 25, padding 25, spacing 25, Font.center ]
                [ paragraph [ regal, fontSize device Lg, Region.heading 4, Font.center ] [ text "Secure Password Generator" ]
                , paragraph [ fontSize device Sm ]
                    [ text "Should it have a number? What about replacing an "
                    , el gray (text "a")
                    , text " with "
                    , el gray (text "@")
                    , text "Don't worry! We have you covered! Just click generate until you see a cryptographically secure password you like. Stop by any time!"
                    ]
                , Input.button [ width fill, alignBottom ]
                    { onPress = Just CopyPass
                    , label =
                        el
                            [ alignBottom
                            , Border.width 3
                            , Border.color purple600
                            , if isPhone then
                                fontSize device Sm

                              else
                                fontSize device Md
                            , Font.color white
                            , Background.color purple600
                            , width fill
                            , paddingXY 0 25
                            , Font.center
                            , regal
                            , Border.rounded 25
                            , transparent
                                (case model.password of
                                    Nothing ->
                                        True

                                    _ ->
                                        False
                                )
                            ]
                            (text (Maybe.withDefault "" model.password))
                    }
                , Input.button [ alignBottom, fontSize device Md, Font.color white, Background.gradient { angle = degrees 45, steps = [ green500, green700 ] }, width fill, padding 25, regalBold, Font.center, Border.rounded 25, htmlAttribute <| class "floatingBtn" ] { onPress = Just GenPass, label = text "Generate!" }
                ]

        checker =
            column [ width fill, height fill, Background.color white, Border.rounded 25, padding 25, spacing 25, Font.center ]
                ([ paragraph [ regal, fontSize device Lg, Region.heading 4, Font.center ] [ text "Data Breach Checker" ]
                 , paragraph [ fontSize device Sm ]
                    [ text "Enter your email here and we will check daily to see if any of your accounts have been compromised. We will only contact you after the first scan is complete, and if you are affected by any future data breaches. No marketing emails. We promise!"
                    ]
                 ]
                    ++ (case model.validEmail of
                            Invalid ->
                                [ Input.email [ onEnter SubmitChecker, alignBottom, fontSize device Md, Border.width 3, Border.color purple600, Font.color muted, Background.color white, width fill, padding 25, Font.center, regal, Border.rounded 25 ] { onChange = CheckerChanged, text = model.checker, placeholder = Just (Input.placeholder [] (text "hello@regaltechsupport.com")), label = Input.labelHidden "Personal of work email" }
                                , Input.button [ alignBottom, fontSize device Md, Font.color white, Background.gradient { angle = degrees 45, steps = [ green500, green700 ] }, width fill, padding 25, regalBold, Font.center, Border.rounded 25, htmlAttribute <| class "floatingBtn" ] { onPress = Just SubmitChecker, label = text "Submit!" }
                                ]

                            Valid ->
                                [ Input.email [ onEnter SubmitChecker, alignBottom, fontSize device Md, Border.width 3, Border.color purple600, Font.color muted, Background.color white, width fill, padding 25, Font.center, regal, Border.rounded 25 ] { onChange = CheckerChanged, text = model.checker, placeholder = Just (Input.placeholder [] (text "hello@regaltechsupport.com")), label = Input.labelHidden "Personal of work email" }
                                , Input.button [ alignBottom, fontSize device Md, Font.color white, Background.gradient { angle = degrees 45, steps = [ green500, green700 ] }, width fill, padding 25, regalBold, Font.center, Border.rounded 25, htmlAttribute <| class "floatingBtn" ] { onPress = Just SubmitChecker, label = text "Submit!" }
                                ]

                            Error ->
                                [ Input.email [ alignBottom, fontSize device Md, Border.width 3, Border.color warning, Font.color muted, Background.color white, width fill, padding 25, Font.center, regal, Border.rounded 25 ] { onChange = CheckerChanged, text = model.checker, placeholder = Just (Input.placeholder [] (text "hello@regaltechsupport.com")), label = Input.labelHidden "Personal of work email" }
                                , Input.button [ alignBottom, fontSize device Md, Font.color white, Background.color muted, width fill, padding 25, regalBold, Font.center, Border.rounded 25 ] { onPress = Nothing, label = text "Please enter a valid email!" }
                                ]
                       )
                )
    in
    el
        [ width fill
        , Background.image "/img/polygons-indigo.svg"
        , clip
        , Border.rounded 25
        , height
            (px
                (if isMobile then
                    1215

                 else
                    720
                )
            )
        ]
        (column [ width fill, paddingEach { top = 0, bottom = 25, left = 25, right = 25 }, spacing 25, height fill ]
            [ column [ alignLeft, Background.color white, padding 25, Border.roundEach { topLeft = 0, topRight = 0, bottomLeft = 25, bottomRight = 25 } ]
                [ el [ regalBold, fontSize device Lg, Region.heading 3 ] (text "Useful Tools!")
                , el [ regal, fontSize device Md ] (text "Always free, enjoy!")
                ]
            , (if isMobile then
                column

               else
                row
              )
                [ width fill, spacing 25 ]
                [ password
                , checker
                ]
            ]
        )


currentMs command =
    Task.perform command Time.now


onEnter : msg -> Element.Attribute msg
onEnter msg =
    htmlAttribute
        (Html.Events.on "keyup"
            (Json.field "key" Json.string
                |> Json.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.succeed msg

                        else
                            Json.fail "Not the enter key"
                    )
            )
        )
