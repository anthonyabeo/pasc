package token

import "fmt"

// Kind defines the category of a token. E.g. Keyword, Identifier etc
type Kind byte

func (k Kind) String() string {
	return tokenKindStrings[k]
}

const (
	EOF Kind = iota

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
	CharString
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
	LessThanGreaterThan
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
}

// NewToken constructs and returns a new token
func NewToken(kind Kind, text string) Token {
	return Token{Kind: kind, Text: text}
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

	Identifier:          "identifier",
	SemiColon:           ";",
	CharString:          "char-string",
	Period:              ".",
	LParen:              "(",
	RParen:              ")",
	Initialize:          ":=",
	Plus:                "+",
	Colon:               ":",
	UIntLiteral:         "uint-literal",
	Comma:               ",",
	Star:                "*",
	FwdSlash:            "/",
	Minus:               "-",
	Equal:               "=",
	LessThan:            "<",
	LessThanGreaterThan: "<>",
	LessThanOrEqual:     "<=",
	GreaterThan:         ">",
	GreaterThanOrEqual:  ">=",
	Range:               "..",
	LSqBrace:            "[",
	RSqBrace:            "]",
	Caret:               "^",
}

func (t *Token) String() string {
	return fmt.Sprintf("<%s>", t.Text)
}

// Keywords define the reserved words of the language
var Keywords map[string]Kind

func init() {
	Keywords = map[string]Kind{
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
