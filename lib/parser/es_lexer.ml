open Es_lexer_helper

type lexer = {
  text : string;
  mutable start_pos : int;
  mutable pos : int;
  mutable token_string_value : string;
}

let init_lexer ~text = { text; start_pos = 0; pos = 0; token_string_value = "" }

exception UnhandledLexerError of string
(* We will have a better error report *)

let utf_decode_at lexer pos =
  let decode = String.get_utf_8_uchar lexer.text pos in
  if not (Uchar.utf_decode_is_valid decode) then
    raise (UnhandledLexerError "Invalid utf-8 code")
  else decode

let code_point_at lexer pos =
  if pos < String.length lexer.text then
    let decode = utf_decode_at lexer pos in
    map_utf_code_to_char_code (Uchar.to_int (Uchar.utf_decode_uchar decode))
  else EndOfFile

let next_code lexer =
  let decode = utf_decode_at lexer lexer.pos in
  lexer.pos <- lexer.pos + Uchar.utf_decode_length decode

(* Step the lexer by <step> length. Only use this if you are sure with the width of characters *)
let next_step lexer step = lexer.pos <- lexer.pos + step

let scan_escape_sequence lexer =
  next_code lexer;

  match code_point_at lexer lexer.pos with
  | Digit '0' ->
      next_code lexer;
      "\x00"
  | Character 'b' ->
      next_code lexer;
      "\b"
  | Character 't' ->
      next_code lexer;
      "\t"
  | Character 'n' ->
      next_code lexer;
      "\n"
  | Character 'v' ->
      next_code lexer;
      "\x0b"
  | Character 'f' ->
      next_code lexer;
      "\x0c"
  | Character 'r' ->
      next_code lexer;
      "\r"
  | SingleQuote ->
      next_code lexer;
      "\'"
  | DoubleQuote ->
      next_code lexer;
      "\""
  | Character 'u' ->
      raise (UnhandledLexerError "Invalid escape sequence \\u")
      (* TODO: Handle unicode *)
  | _ -> raise (UnhandledLexerError "Invalid escape sequence")

let scan_string lexer =
  let quote = code_point_at lexer lexer.pos in
  next_code lexer;

  let is_complete = ref false in
  let plain_start = ref lexer.pos in

  let buf = Buffer.create 16 in

  while not !is_complete do
    match code_point_at lexer lexer.pos with
    | Backslash ->
        Buffer.add_string buf
          (String.sub lexer.text !plain_start (lexer.pos - !plain_start));
        let escaped = scan_escape_sequence lexer in
        Buffer.add_string buf escaped;
        plain_start := lexer.pos
    | (SingleQuote | DoubleQuote) as c ->
        if quote = c then (
          Buffer.add_string buf
            (String.sub lexer.text !plain_start (lexer.pos - !plain_start));
          is_complete := true);
        next_code lexer
    | _ -> next_code lexer
  done;

  Buffer.contents buf

let scan lexer =
  lexer.start_pos <- lexer.pos;

  match code_point_at lexer lexer.pos with
  | Hash -> (
      match code_point_at lexer (lexer.pos + 1) with
      | Exclamation ->
          (* Proposal Hashbang https://github.com/tc39/proposal-hashbang *)
          while
            not
              (let code_point = code_point_at lexer lexer.pos in
               code_point = EndOfFile || is_line_terminator code_point)
          do
            next_code lexer
          done;
          Es_ast.ShebangTrivia
      | _ -> raise (UnhandledLexerError "Expect a token '!#'"))
  | Tab | VerticalTab | FormFeed | Space | NonBreakingSpace | Ogham | EnQuad
  | EmQuad | EnSpace | EmSpace | ThreePerEmSpace | FourPerEmSpace
  | SixPerEmSpace | FigureSpace | PunctuationSpace | ThinSpace | HairSpace
  | ZeroWidthSpace | NarrowNoBreakSpace | MathematicalSpace | IdeographicSpace
  | ByteOrderMark ->
      while is_whitespace (code_point_at lexer lexer.pos) do
        next_code lexer
      done;

      Es_ast.WhitespaceTrivia
  | Exclamation
    when code_point_at lexer (lexer.pos + 1) = Equals
         && code_point_at lexer (lexer.pos + 2) = Equals ->
      next_step lexer 3;
      Es_ast.ExclamationEqualsEqualsToken
  | Exclamation when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      Es_ast.ExclamationEqualsToken
  | Exclamation ->
      next_step lexer 1;
      Es_ast.ExclamationToken
  | DoubleQuote | SingleQuote ->
      lexer.token_string_value <- scan_string lexer;
      StringLiteral
  | Backtick ->
      raise (UnhandledLexerError "Backtick") (* TODO: Template Literal *)
  | Percent when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      PercentEqualsToken
  | Percent ->
      next_step lexer 1;
      PercentToken
  | Ampersand
    when code_point_at lexer (lexer.pos + 1) = Ampersand
         && code_point_at lexer (lexer.pos + 2) = Equals ->
      next_step lexer 3;
      AmpersandAmpersandEqualsToken
  | Ampersand when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      AmpersandEqualsToken
  | Ampersand when code_point_at lexer (lexer.pos + 1) = Ampersand ->
      next_step lexer 2;
      AmpersandAmpersandToken
  | Ampersand ->
      next_step lexer 1;
      AmpersandToken
  | OpenParen ->
      next_step lexer 1;
      OpenParenToken
  | CloseParen ->
      next_step lexer 1;
      CloseParenToken
  | Asterisk when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      AsteriskEqualsToken
  | Asterisk
    when code_point_at lexer (lexer.pos + 1) = Asterisk
         && code_point_at lexer (lexer.pos + 2) = Equals ->
      next_step lexer 3;
      AsteriskAsteriskEqualsToken
  | Asterisk when code_point_at lexer (lexer.pos + 1) = Asterisk ->
      next_step lexer 2;
      AsteriskAsteriskToken
  | Asterisk ->
      next_step lexer 1;
      AsteriskToken
  | Plus when code_point_at lexer (lexer.pos + 1) = Plus ->
      next_step lexer 2;
      PlusPlusToken
  | Plus when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      PlusEqualsToken
  | Comma ->
      next_step lexer 1;
      CommaToken
  | Minus when code_point_at lexer (lexer.pos + 1) = Minus ->
      next_step lexer 2;
      MinusMinusToken
  | Minus when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      MinusEqualsToken
  | Dot -> (
      match code_point_at lexer (lexer.pos + 1) with
      | Digit _ -> raise (UnhandledLexerError "Invalid code")
      | Dot when code_point_at lexer (lexer.pos + 2) = Dot ->
          next_step lexer 3;
          DotDotDotToken
      | _ ->
          next_step lexer 1;
          DotToken)
  | Slash ->
      next_step lexer 1;
      SlashToken (* TODO: Comments *)
  | Digit _ ->
      raise (UnhandledLexerError "Invalid code") (* TODO: Number Literal *)
  | Colon ->
      next_step lexer 1;
      ColonToken
  | Semicolon ->
      next_step lexer 1;
      SemicolonToken
  | LessThan -> raise (UnhandledLexerError "Invalid code") (* TODO: LessThan *)
  | Equals
    when code_point_at lexer (lexer.pos + 1) = Equals
         && code_point_at lexer (lexer.pos + 2) = Equals ->
      next_step lexer 3;
      EqualsEqualsEqualsToken
  | Equals when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      EqualsEqualsToken
  | Equals ->
      next_step lexer 1;
      EqualsToken
  | GreaterThan ->
      raise (UnhandledLexerError "Invalid code") (* TODO: GreaterThan *)
  | Question
    when code_point_at lexer (lexer.pos + 1) = Dot
         && not (is_digit (code_point_at lexer (lexer.pos + 2))) ->
      (* Parsing '?.'. If there is "null ?.1", it should be treated as "null ? .1" *)
      next_step lexer 2;
      QuestionDotToken
  | Question
    when code_point_at lexer (lexer.pos + 1) = Question
         && code_point_at lexer (lexer.pos + 2) = Equals ->
      next_step lexer 3;
      QuestionToken
  | Question when code_point_at lexer (lexer.pos + 1) = Question ->
      next_step lexer 2;
      QuestionToken
  | Question ->
      next_step lexer 1;
      QuestionToken
  | OpenBracket ->
      next_step lexer 1;
      OpenBracketTokens
  | CloseBracket ->
      next_step lexer 1;
      CloseBracketToken
  | Caret when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      CaretEqualsToken
  | Caret ->
      next_step lexer 1;
      CaretToken
  | OpenBrace ->
      next_step lexer 1;
      OpenBraceToken
  | Bar
    when code_point_at lexer (lexer.pos + 1) = Bar
         && code_point_at lexer (lexer.pos + 2) = Equals ->
      next_step lexer 3;
      BarBarEqualsToken
  | Bar when code_point_at lexer (lexer.pos + 1) = Equals ->
      next_step lexer 2;
      BarEqualsToken
  | Bar ->
      next_step lexer 1;
      BarToken
  | CloseBrace ->
      next_step lexer 1;
      CloseBraceToken
  | Tilde ->
      next_step lexer 1;
      TildeToken
  | At ->
      next_step lexer 1;
      AtToken
  | Backslash -> raise (UnhandledLexerError "Invalid code") (* TODO *)
  | _ -> raise (UnhandledLexerError "Invalid code") (* TODO: Identifier *)
