module ByteArray exposing (..)

import Native.ByteArray
import Bitwise


type ByteArray
    = ByteArray


empty : ByteArray
empty =
    Native.ByteArray.empty


get : Int -> ByteArray -> Maybe Int
get i arr =
    Native.ByteArray.get i arr


get16 : Int -> ByteArray -> Maybe Int
get16 i arr =
    Maybe.map2
        (\low_byte high_byte -> Bitwise.or low_byte (Bitwise.shiftLeftBy 8 high_byte))
        (get i arr)
        (get (i + 1) arr)


fromBase64 : String -> Result String ByteArray
fromBase64 base64 =
    Native.ByteArray.fromBase64 base64


slice : Int -> Int -> ByteArray -> ByteArray
slice start end arr =
    Native.ByteArray.slice start end arr


toList : ByteArray -> List Int
toList arr =
    Native.ByteArray.toList arr


getByte : Int -> ByteArray -> Int
getByte addr memory =
    Maybe.withDefault 0 (get addr memory)


getWord : Int -> ByteArray -> Int
getWord addr memory =
    Maybe.withDefault 0 (get16 addr memory)
