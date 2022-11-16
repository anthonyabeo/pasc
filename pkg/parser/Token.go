package parser

import "fmt"

// TokenType defines the category of a token. E.g. Keyword, Identifier etc
type TokenType byte

const (
	// EOF ...
	EOF TokenType = iota
)

// Token defines a type of token
type Token struct {
	Text string
	Type TokenType
}

// GetTokenName returns the textual form of a token given its TokenType value
func (t *Token) GetTokenName() string {
	switch t.Type {
	case 1:
		return "EOF"
	default:
		return ""
	}
}

func (t *Token) String() string {
	return fmt.Sprintf("<%s, %s>", t.Text, t.GetTokenName())
}
