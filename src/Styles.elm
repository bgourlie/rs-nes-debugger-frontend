module Styles exposing (helpers, class, id, withClass, Class(..), Id(..))

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


withClass : Class -> List Css.Mixin -> Css.Mixin
withClass class mixins =
    Css.withClass class mixins


type Class
    = MessageRepeats
    | MessageRepeatsShow
    | RowOffset
    | OffsetColumn
    | BytesRow
    | ButtonIcon
    | BreakpointIcon
    | InstructionGutter
    | InstructionValue
    | BreakpointHitBox
    | BreakpointOn
    | MemoryLocation
    | AddressModeValues
    | AddressModeMemoryLocation
    | AddressModeMemoryValue
    | Mnemonic
    | Operand
    | UndefinedOpcode
    | CurrentInstruction
    | Instruction
    | ConsoleLine
    | DebuggerConnected
    | DebuggerNotConnected
    | ConsoleInputDisplayed


type Id
    = Container
    | TwoColumn
    | LeftColumn
    | ConsoleContainer
    | HexEditorContainer
    | RightColumn
    | ConsoleLines
    | ConsoleInput
    | HexEditor
    | HexEditorBody
    | InstructionsContainer
    | Instructions
    | StatusStrip
    | DebuggerStatus
    | Registers
