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
	Goto
	Div
	Mod
	And
	Or
	In
	Function
	Procedure
	If

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
	Star
	FwdSlash
	Minus
	Equal
	LessThan
	LessThanGreaterThan
	LessThanOrEqual
	GreaterThan
	GreaterThanOrEqual
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
	case 6:
		return "goto"
	case 7:
		return "div"
	case 8:
		return "mod"
	case 9:
		return "and"
	case 10:
		return "or"
	case 11:
		return "in"
	case 12:
		return "function"
	case 13:
		return "procedure"
	case 14:
		return "if"
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
	}
}
