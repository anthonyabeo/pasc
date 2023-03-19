package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// StringLiteral defines a string value node in the AST
type StringLiteral struct {
	token    token.Token
	value    string
	EvalType types.Type
}

// NewStringLiteral creates and returns a new StringLiteral
func NewStringLiteral(token token.Token, value string) *StringLiteral {
	return &StringLiteral{token: token, value: value}
}

// TokenLiteral returns the text value this node's token.
func (sl *StringLiteral) TokenLiteral() string {
	return sl.token.Text
}

// TokenKind ...
func (sl *StringLiteral) TokenKind() token.Kind {
	return sl.token.Kind
}

func (sl *StringLiteral) exprNode() {}

// Attr ...
func (sl *StringLiteral) Attr(attr string) any {
	switch attr {
	case "type":
		return sl.EvalType
	default:
		return ""
	}
}

func (sl *StringLiteral) String() string {
	return sl.value
}
