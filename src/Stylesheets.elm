port module Stylesheets exposing (..)

import Css.File exposing (..)
import AppCss


port files : CssFileStructure -> Cmd msg


fileStructure : CssFileStructure
fileStructure =
    Css.File.toFileStructure
        [ ( "styles.css", Css.File.compile [ AppCss.css ] ) ]


main : CssCompilerProgram
main =
    Css.File.compiler files fileStructure
