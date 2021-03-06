module Colors exposing (..)

import Css exposing (hex)


background : Css.Color
background =
    hex "#2b2b2b"


foreground : Css.Color
foreground =
    hex "#a9b7c6"


consoleBackground : Css.Color
consoleBackground =
    hex "#111111"


consoleInputBackground : Css.Color
consoleInputBackground =
    hex "#111111"


consoleInputText : Css.Color
consoleInputText =
    hex "#dddddd"


currentLine : Css.Color
currentLine =
    hex "#2d6099"


mnemonic : Css.Color
mnemonic =
    hex "#a5c25c"


undefinedOpcode : Css.Color
undefinedOpcode =
    hex "#606366"


gutterBackground : Css.Color
gutterBackground =
    hex "#313335"


gutterBorder : Css.Color
gutterBorder =
    hex "#606366"


lineNumber : Css.Color
lineNumber =
    hex "#606366"


headerColor : Css.Color
headerColor =
    hex "#313335"


headerBorder : Css.Color
headerBorder =
    hex "#606366"


messageRepeatBackgroundColor : Css.Color
messageRepeatBackgroundColor =
    hex "#214283"


breakpointColor : String
breakpointColor =
    "#FF6666"


buttonBorderColor : String
buttonBorderColor =
    "#808080"


debuggerIconColor : String
debuggerIconColor =
    "#9FD6AE"


hexEditorOffsetColor : Css.Color
hexEditorOffsetColor =
    hex "#606366"


hexEditorByte : Css.Color
hexEditorByte =
    hex "#6897bb"


hexEditorBackground : Css.Color
hexEditorBackground =
    hex "#222222"


addressModeActiveValue : Css.Color
addressModeActiveValue =
    hex "#ffc66d"


addressModeInactiveValue : Css.Color
addressModeInactiveValue =
    hex "#777777"


statusConnected : Css.Color
statusConnected =
    hex "00aa00"


statusDisconnected : Css.Color
statusDisconnected =
    hex "990000"


statusStripBackgroundColor : Css.Color
statusStripBackgroundColor =
    hex "3c3f41"
