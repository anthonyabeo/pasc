package token

// Kind defines the category of a token. E.g. Keyword, Identifier etc
type Kind byte

func (k Kind) String() string {
	return tokenKindStrings[k]
}

const (
	Illegal Kind = iota
	EOF

	// Keywords
	Program
	Begin
	End
	Var
	Integer
	Goto
	Div
	Mod
	And
	Or
	In
	Function
	Procedure
	If
	Then
	Else
	With
	For
	Repeat
	While
	Case
	Nil
	Not
	To
	DownTo
	Do
	Const
	Until
	Of
	Type
	Array
	Record
	File
	Set
	Packed
	Real
	Boolean
	Char
	Label
	True
	False

	Identifier
	SemiColon
	StrLiteral
	Period
	LParen
	RParen
	Initialize
	Plus
	Colon
	UIntLiteral
	Comma
	Star
	FwdSlash
	Minus
	Equal
	LessThan
	NotEqual
	LessThanOrEqual
	GreaterThan
	GreaterThanOrEqual
	URealLiteral
	Range
	LSqBrace
	RSqBrace
	Caret
	Return
	Input
	Output
)

// Token defines a type of token
type Token struct {
	Text string
	Kind Kind
	Pos  *Position
}

// NewToken constructs and returns a new token
func NewToken(kind Kind, text string, pos *Position) Token {
	return Token{Kind: kind, Text: text, Pos: pos}
}

func (t *Token) String() string {
	return t.Text
}

var tokenKindStrings = [...]string{
	EOF:       "EOF",
	Program:   "program",
	Begin:     "begin",
	End:       "end",
	Var:       "var",
	Integer:   "integer",
	Goto:      "goto",
	Div:       "div",
	Mod:       "mod",
	And:       "and",
	Or:        "or",
	In:        "in",
	Function:  "function",
	Procedure: "procedure",
	If:        "if",
	Then:      "then",
	Else:      "else",
	With:      "with",
	For:       "for",
	Repeat:    "repeat",
	While:     "while",
	Case:      "case",
	Nil:       "nil",
	Not:       "not",
	To:        "to",
	DownTo:    "downto",
	Do:        "do",
	Const:     "const",
	Until:     "until",
	Of:        "of",
	Type:      "type",
	Array:     "array",
	Record:    "record",
	File:      "file",
	Set:       "set",
	Packed:    "packed",
	Real:      "real",
	Boolean:   "Boolean",
	Char:      "char",
	Label:     "label",
	True:      "true",
	False:     "false",
	Return:    "return",
	Input:     "input",
	Output:    "output",

	Identifier:         "identifier",
	SemiColon:          ";",
	StrLiteral:         "str-literal",
	Period:             ".",
	LParen:             "(",
	RParen:             ")",
	Initialize:         ":=",
	Plus:               "+",
	Colon:              ":",
	UIntLiteral:        "uint-literal",
	Comma:              ",",
	Star:               "*",
	FwdSlash:           "/",
	Minus:              "-",
	Equal:              "=",
	LessThan:           "<",
	NotEqual:           "<>",
	LessThanOrEqual:    "<=",
	GreaterThan:        ">",
	GreaterThanOrEqual: ">=",
	Range:              "..",
	LSqBrace:           "[",
	RSqBrace:           "]",
	Caret:              "^",
}

// Keywords define the reserved words of the language
var keywords map[string]Kind

func init() {
	keywords = map[string]Kind{
		"program":   Program,
		"begin":     Begin,
		"end":       End,
		"integer":   Integer,
		"var":       Var,
		"div":       Div,
		"mod":       Mod,
		"and":       And,
		"or":        Or,
		"in":        In,
		"function":  Function,
		"procedure": Procedure,
		"if":        If,
		"then":      Then,
		"else":      Else,
		"with":      With,
		"for":       For,
		"repeat":    Repeat,
		"while":     While,
		"case":      Case,
		"nil":       Nil,
		"not":       Not,
		"to":        To,
		"downto":    DownTo,
		"do":        Do,
		"const":     Const,
		"until":     Until,
		"of":        Of,
		"type":      Type,
		"array":     Array,
		"record":    Record,
		"file":      File,
		"set":       Set,
		"packed":    Packed,
		"real":      Real,
		"Boolean":   Boolean,
		"char":      Char,
		"label":     Label,
		"true":      True,
		"false":     False,
		"goto":      Goto,
	}
}

// Lookup maps an identifier to its keyword token or IDENT (if not a keyword).
func Lookup(ident string) Kind {
	if tok, isKeyword := keywords[ident]; isKeyword {
		return tok
	}

	return Identifier
}
