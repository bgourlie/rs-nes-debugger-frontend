module Styles exposing (helpers, class, id, Class(..), Id(..))

import Html.CssHelpers
import Css exposing ((.), (#))


helpers : Html.CssHelpers.Namespace String Class Id msg
helpers =
    Html.CssHelpers.withNamespace ""


class : Class -> List Css.Mixin -> Css.Snippet
class classType mixins =
    (.) classType mixins


id : Id -> List Css.Mixin -> Css.Snippet
id idType mixins =
    (#) idType mixins


type Class
    = Button
    | MessageRepeats
    | MessageRepeatsShow
    | RowOffset
    | OffsetColumn
    | BytesRow
    | ButtonIcon
    | BreakpointIcon
    | Gutter
    | BreakpointHitBox
    | BreakpointOn
    | MemoryLocation
    | AddressModeValues
    | AddressModeMemoryLocation
    | AddressModeMemoryValue
    | Mnemonic
    | Operand
    | UndefinedOpcode
    | Instructions
    | CurrentInstruction
    | ConsoleLine
    | DebuggerConnected
    | DebuggerNotConnected


type Id
    = Container
    | TwoColumn
    | DebuggerButtons
    | LeftColumn
    | ConsoleContainer
    | HexEditorContainer
    | RightColumn
    | ByteFormatToggle
    | Console
    | ConsoleLines
    | ConsoleInput
    | HexEditor
    | HexEditorBody
    | InstructionsContainer
    | Cycles
    | StatusStrip
    | HeaderControls
