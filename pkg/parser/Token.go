package parser

import "fmt"

// TokenType defines the category of a token. E.g. Keyword, Identifier etc
type TokenType byte

const (
	Eof TokenType = iota

	// Keywords
	Program
	Begin
	End

	Identifier
	SemiColon
	StrLiteral
	Period
	LParen
	RParen
)

// Token defines a type of token
type Token struct {
	Text string
	Type TokenType
}

// GetTokenName returns the textual form of a token given its TokenType value
func GetTokenName(t byte) string {
	switch t {
	case 1:
		return "EOF"
	case 2:
		return "Program"
	case 3:
		return "Begin"
	case 4:
		return "End"
	default:
		return ""
	}
}

func (t *Token) String() string {
	return fmt.Sprintf("<%s, %s>", t.Text, GetTokenName(byte(t.Type)))
}

// Keywords define the reserved words of the language
var Keywords map[string]TokenType

func init() {
	Keywords = map[string]TokenType{
		"program": Program,
		"begin":   Begin,
		"end":     End,
	}
}
