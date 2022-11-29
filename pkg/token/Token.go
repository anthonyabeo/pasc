package token

import "fmt"

// Kind defines the category of a token. E.g. Keyword, Identifier etc
type Kind byte

const (
	EOF Kind = iota

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
	Kind Kind
}

// GetTokenName returns the textual form of a token given its TokenType value
func GetTokenName(t Kind) string {
	switch t {
	case 0:
		return "EOF"
	case 1:
		return "program"
	case 2:
		return "begin"
	case 3:
		return "end"
	case 4:
		return "var"
	case 5:
		return "integer"
	default:
		return ""
	}
}

func (t *Token) String() string {
	return fmt.Sprintf("<%s, %s>", t.Text, GetTokenName(t.Kind))
}

// Keywords define the reserved words of the language
var Keywords map[string]Kind

func init() {
	Keywords = map[string]Kind{
		"program": Program,
		"begin":   Begin,
		"end":     End,
		"integer": Integer,
		"var":     Var,
	}
}
