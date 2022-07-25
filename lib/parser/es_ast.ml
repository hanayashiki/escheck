type syntax_kind =
  | Unknown
  | EndOfFileToken
  | SingleLineCommentTrivia
  | MultiLineCommentTrivia
  | NewLineTrivia
  | WhitespaceTrivia (* We detect and preserve #! on the first line*)
  | ShebangTrivia
    (* We detect and provide better error recovery when we encounter a git merge marker.
        This allows us to edit files with git-conflict markers in them in a much more pleasant manner.*)
  | ConflictMarkerTrivia (* Literals *)
  | NumericLiteral
  | BigIntLiteral
  | StringLiteral
  | JsxText
  | JsxTextAllWhiteSpaces
  | RegularExpressionLiteral
  | NoSubstitutionTemplateLiteral (* Pseudo-literals *)
  | TemplateHead
  | TemplateMiddle
  | TemplateTail (* Punctuation *)
  | OpenBraceToken
  | CloseBraceToken
  | OpenParenToken
  | CloseParenToken
  | OpenBracketTokens
  | CloseBracketToken
  | DotToken
  | DotDotDotToken
  | SemicolonToken
  | CommaToken
  | QuestionDotToken
  | LessThanToken
  | LessThanSlashToken
  | GreaterThanToken
  | LessThanEqualsToken
  | GreaterThanEqualsToken
  | EqualsEqualsToken
  | ExclamationEqualsToken
  | EqualsEqualsEqualsToken
  | ExclamationEqualsEqualsToken
  | EqualsGreaterThanToken
  | PlusToken
  | MinusToken
  | AsteriskToken
  | AsteriskAsteriskToken
  | SlashToken
  | PercentToken
  | PlusPlusToken
  | MinusMinusToken
  | LessThanLessThanToken
  | GreaterThanGreaterThanToken
  | GreaterThanGreaterThanGreaterThanToken
  | AmpersandToken
  | BarToken
  | CaretToken
  | ExclamationToken
  | TildeToken
  | AmpersandAmpersandToken
  | BarBarToken
  | QuestionToken
  | ColonToken
  | AtToken
  | QuestionQuestionToken
      (** Only the JSDoc scanner produces BacktickToken. The normal scanner produces NoSubstitutionTemplateLiteral and related kinds. *)
  | BacktickToken
      (** Only the JSDoc scanner produces HashToken. The normal scanner produces PrivateIdentifier. *)
  | HashToken  (** Assignments *)
  | EqualsToken
  | PlusEqualsToken
  | MinusEqualsToken
  | AsteriskEqualsToken
  | AsteriskAsteriskEqualsToken
  | SlashEqualsToken
  | PercentEqualsToken
  | LessThanLessThanEqualsToken
  | GreaterThanGreaterThanEqualsToken
  | GreaterThanGreaterThanGreaterThanEqualsToken
  | AmpersandEqualsToken
  | BarEqualsToken
  | BarBarEqualsToken
  | AmpersandAmpersandEqualsToken
  | QuestionQuestionEqualsToken
  | CaretEqualsToken
