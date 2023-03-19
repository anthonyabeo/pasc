package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
)

// BoolLiteral defines a boolean value node in the AST
type BoolLiteral struct {
	token token.Token
	value string
}

// NewBoolLiteral creates and returns a new StringLiteral
func NewBoolLiteral(token token.Token, value string) *BoolLiteral {
	return &BoolLiteral{token: token, value: value}
}

// TokenLiteral returns the text value this node's token.
func (b *BoolLiteral) TokenLiteral() string {
	return b.token.Text
}

// TokenKind ...
func (b *BoolLiteral) TokenKind() token.Kind {
	return b.token.Kind
}

func (b *BoolLiteral) String() string {
	return b.value
}

// Value ...
func (b *BoolLiteral) Value() string {
	return b.value
}

// Type ...
func (b *BoolLiteral) Type() types.Type {
	return &base.Boolean{Name: "Boolean"}
}
