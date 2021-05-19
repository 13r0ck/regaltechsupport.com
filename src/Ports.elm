port module Ports exposing (..)

import Json.Decode as Json



-- Ports


port copyText : String -> Cmd msg


port successfulCopy : (Bool -> msg) -> Sub msg
