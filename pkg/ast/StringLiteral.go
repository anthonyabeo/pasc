package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
)

// StringLiteral defines a string value node in the AST
type StringLiteral struct {
	token token.Token
	value string
}

// NewStringLiteral creates and returns a new StringLiteral
func NewStringLiteral(token token.Token, value string) *StringLiteral {
	return &StringLiteral{token: token, value: value}
}

// TokenLiteral returns the text value this node's token.
func (sl *StringLiteral) TokenLiteral() string {
	return sl.token.Text
}

func (sl *StringLiteral) exprNode() {}

func (sl *StringLiteral) String() string {
	return sl.value
}
