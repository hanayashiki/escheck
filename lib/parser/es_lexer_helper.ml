type char_codes =
  | EndOfFile
  | NullCharacter
  | MaxAsciiCharacter
  | LineFeed (* \n *)
  | CarriageReturn (* \r *)
  | LineSeparator
  | ParagraphSeparator
  | NextLine
  | Space
  | NonBreakingSpace
  (* Unicode 3.0 space characters *)
  | EnQuad
  | EmQuad
  | EnSpace
  | EmSpace
  | ThreePerEmSpace
  | FourPerEmSpace
  | SixPerEmSpace
  | FigureSpace
  | PunctuationSpace
  | ThinSpace
  | HairSpace
  | ZeroWidthSpace
  | NarrowNoBreakSpace
  | IdeographicSpace
  | MathematicalSpace
  | Ogham
  | Underline
  | Dollar
  | Digit of char
  | Character of char
  | Ampersand
  | Asterisk
  | At
  | Backslash
  | Backtick
  | Bar
  | Caret
  | CloseBrace
  | CloseBracket
  | CloseParen
  | Colon
  | Comma
  | Dot
  | DoubleQuote
  | Equals
  | Exclamation
  | GreaterThan
  | Hash
  | LessThan
  | Minus
  | OpenBrace
  | OpenBracket
  | OpenParen
  | Percent
  | Plus
  | Question
  | Semicolon
  | SingleQuote
  | Slash
  | Tilde
  | Backspace
  | FormFeed
  | ByteOrderMark
  | Tab
  | VerticalTab
  | Unicode of int
[@@deriving show]

let map_utf_code_to_char_code = function
  | 0 -> NullCharacter
  | 0x7f -> MaxAsciiCharacter
  | 0x0A -> LineFeed
  | 0x0D -> CarriageReturn
  | 0x2028 -> LineSeparator
  | 0x2029 -> ParagraphSeparator
  | 0x0085 -> NextLine
  | 0x0020 -> Space
  | 0x00a0 -> NonBreakingSpace
  | 0x2000 -> EnQuad
  | 0x2001 -> EmQuad
  | 0x2002 -> EnSpace
  | 0x2003 -> EmSpace
  | 0x2004 -> ThreePerEmSpace
  | 0x2005 -> FourPerEmSpace
  | 0x2006 -> SixPerEmSpace
  | 0x2007 -> FigureSpace
  | 0x2008 -> PunctuationSpace
  | 0x2009 -> ThinSpace
  | 0x200A -> HairSpace
  | 0x200B -> ZeroWidthSpace
  | 0x202F -> NarrowNoBreakSpace
  | 0x3000 -> IdeographicSpace
  | 0x205F -> MathematicalSpace
  | 0x1680 -> Ogham
  | 0x5F -> Underline
  | 0x24 -> Dollar
  | (0x30 | 0x31 | 0x32 | 0x33 | 0x34 | 0x35 | 0x36 | 0x37 | 0x38 | 0x39) as c
    ->
      Digit (char_of_int c)
  | ( 0x61 | 0x62 | 0x63 | 0x64 | 0x65 | 0x66 | 0x67 | 0x68 | 0x69 | 0x6a | 0x6b
    | 0x6c | 0x6d | 0x6e | 0x6f | 0x70 | 0x71 | 0x72 | 0x73 | 0x74 | 0x75 | 0x76
    | 0x77 | 0x78 | 0x79 | 0x7A ) as c ->
      Character (char_of_int c)
  | ( 0x41 | 0x42 | 0x43 | 0x44 | 0x45 | 0x46 | 0x47 | 0x48 | 0x49 | 0x4a | 0x4b
    | 0x4c | 0x4d | 0x4e | 0x4f | 0x50 | 0x51 | 0x52 | 0x53 | 0x54 | 0x55 | 0x56
    | 0x57 | 0x58 | 0x59 | 0x5a ) as c ->
      Character (char_of_int c)
  | 0x26 -> Ampersand
  | 0x2a -> Asterisk
  | 0x40 -> At
  | 0x5c -> Backslash
  | 0x60 -> Backtick
  | 0x7c -> Bar
  | 0x5e -> Caret
  | 0x7d -> CloseBrace
  | 0x5d -> CloseBracket
  | 0x29 -> CloseParen
  | 0x3a -> Colon
  | 0x2c -> Comma
  | 0x2e -> Dot
  | 0x22 -> DoubleQuote
  | 0x3d -> Equals
  | 0x21 -> Exclamation
  | 0x3e -> GreaterThan
  | 0x23 -> Hash
  | 0x3c -> LessThan
  | 0x2d -> Minus
  | 0x7b -> OpenBrace
  | 0x5b -> OpenBracket
  | 0x28 -> OpenParen
  | 0x25 -> Percent
  | 0x2b -> Plus
  | 0x3f -> Question
  | 0x3b -> Semicolon
  | 0x27 -> SingleQuote
  | 0x2f -> Slash
  | 0x7e -> Tilde
  | 0x08 -> Backspace
  | 0x0c -> FormFeed
  | 0xfeff -> ByteOrderMark
  | 0x09 -> Tab
  | 0x0b -> VerticalTab
  | c -> Unicode c

let is_line_terminator = function
  | LineFeed | CarriageReturn | LineSeparator | ParagraphSeparator -> true
  | _ -> false

let is_whitespace = function
  | Tab | VerticalTab | FormFeed | Space | NonBreakingSpace | Ogham | EnQuad
  | EmQuad | EnSpace | EmSpace | ThreePerEmSpace | FourPerEmSpace
  | SixPerEmSpace | FigureSpace | PunctuationSpace | ThinSpace | HairSpace
  | ZeroWidthSpace | NarrowNoBreakSpace | MathematicalSpace | IdeographicSpace
  | ByteOrderMark ->
      true
  | _ -> false

let is_digit = function Digit _ -> true | _ -> false

let is_identifier_start = function
  | Character _ | Dollar | Underline | Digit _ -> true
  | Unicode c -> Uucp.Id.is_id_start (Uchar.of_int c)
  | _ -> false

let is_identifer_part = function
  | Character _ | Dollar | Underline -> true
  | Unicode c -> Uucp.Id.is_id_continue (Uchar.of_int c)
  | _ -> false
