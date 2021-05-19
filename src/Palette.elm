module Palette exposing (FontSize(..), black, blue500, fontSize, green300, green500, green700, maxWidth, muted, purple600, regal, regalBold, warning, white)

import Element exposing (..)
import Element.Font as Font


type FontSize
    = Xsm
    | Sm
    | Md
    | Lg
    | Xlg
    | XXlg


fontSize : DeviceClass -> FontSize -> Attr decorative msg
fontSize device size =
    Font.size
        (case device of
            Phone ->
                case size of
                    Xsm ->
                        13

                    Sm ->
                        20

                    Md ->
                        25

                    Lg ->
                        38

                    Xlg ->
                        45

                    XXlg ->
                        45

            Tablet ->
                case size of
                    Xsm ->
                        13

                    Sm ->
                        20

                    Md ->
                        25

                    Lg ->
                        38

                    Xlg ->
                        45

                    XXlg ->
                        60

            Desktop ->
                case size of
                    Xsm ->
                        18

                    Sm ->
                        25

                    Md ->
                        30

                    Lg ->
                        43

                    Xlg ->
                        50

                    XXlg ->
                        80

            BigDesktop ->
                case size of
                    Xsm ->
                        18

                    Sm ->
                        25

                    Md ->
                        30

                    Lg ->
                        43

                    Xlg ->
                        50

                    XXlg ->
                        80
        )


maxWidth : number
maxWidth =
    2000


white : Color
white =
    rgb 1 1 1


warning : Color
warning =
    rgb255 204 51 51


black : Color
black =
    rgb 0 0 0


muted : Color
muted =
    rgb255 31 41 55


green300 : Color
green300 =
    rgb255 110 231 183


green500 : Color
green500 =
    rgb255 16 185 129


green700 : Color
green700 =
    rgb255 4 120 87


blue500 : Color
blue500 =
    rgb255 59 130 246


purple600 : Color
purple600 =
    rgb255 124 58 237


regalBold : Attribute msg
regalBold =
    Font.family [ Font.typeface "regalBold", Font.sansSerif ]


regal : Attribute msg
regal =
    Font.family [ Font.typeface "regal", Font.sansSerif ]
