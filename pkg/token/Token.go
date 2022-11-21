package token

import "fmt"

// Type defines the category of a token. E.g. Keyword, Identifier etc
type Type byte

const (
	EOF Type = iota

	// Keywords
	Program
	Begin
	End
	Var
	Integer

	Identifier
	SemiColon
	StrLiteral
	Period
	LParen
	RParen
	Initialize
	Plus
	Colon
	IntLiteral
	Comma
)

// Token defines a type of token
type Token struct {
	Text string
	Type Type
}

// GetTokenName returns the textual form of a token given its TokenType value
func GetTokenName(t Type) string {
	switch t {
	case 1:
		return "EOF"
	case 2:
		return "program"
	case 3:
		return "begin"
	case 4:
		return "end"
	case 5:
		return "var"
	case 6:
		return "integer"
	default:
		return ""
	}
}

func (t *Token) String() string {
	return fmt.Sprintf("<%s, %s>", t.Text, GetTokenName(t.Type))
}

// Keywords define the reserved words of the language
var Keywords map[string]Type

func init() {
	Keywords = map[string]Type{
		"program": Program,
		"begin":   Begin,
		"end":     End,
		"integer": Integer,
		"var":     Var,
	}
}
