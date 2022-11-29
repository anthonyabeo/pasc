package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// IntegerLiteral ...
type IntegerLiteral struct {
	Token token.Token
	Value string
}

// NewIntegerLiteral ...
func NewIntegerLiteral(tt token.Token) *IntegerLiteral {
	return &IntegerLiteral{Token: tt, Value: tt.Text}
}

// TokenLiteral returns the text value this node's token.
func (l *IntegerLiteral) TokenLiteral() string {
	return l.Token.Text
}

func (l *IntegerLiteral) exprNode() {}

func (l *IntegerLiteral) String() string {
	return l.Value
}
