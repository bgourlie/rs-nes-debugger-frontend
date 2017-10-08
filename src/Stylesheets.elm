port module Stylesheets exposing (..)

import AppCss
import Css.File exposing (..)


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "styles.css", Css.File.compile [ AppCss.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
