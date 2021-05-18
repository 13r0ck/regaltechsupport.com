module Palette exposing (FontSize(..), black, blue500, fontSize, green300, maxWidth, muted, purple600, warning, white)

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


blue500 : Color
blue500 =
    rgb255 59 130 246


purple600 : Color
purple600 =
    rgb255 124 58 237
