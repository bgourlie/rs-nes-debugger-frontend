module ByteArray exposing (..)

import Native.ByteArray


type ByteArray
    = ByteArray


empty : ByteArray
empty =
    Native.ByteArray.empty


get : Int -> ByteArray -> Maybe Int
get i arr =
    Native.ByteArray.get i arr


fromBase64 : String -> Result String ByteArray
fromBase64 base64 =
    Native.ByteArray.fromBase64 base64


slice : Int -> Int -> ByteArray -> ByteArray
slice start end arr =
    Native.ByteArray.slice start end arr


toList : ByteArray -> List Int
toList arr =
    Native.ByteArray.toList arr
