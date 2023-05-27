package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
)

// BoolExpression defines a boolean value node in the AST
type BooleanExpression struct {
	token token.Token
	value string
}

// NewBoolLiteral creates and returns a new StringLiteral
//func NewBoolLiteral(token token.Token, value string) *BooleanExpression {
//	return &BooleanExpression{token: token, value: value}
//}

// TokenLiteral returns the text value this node's token.
func (b *BooleanExpression) TokenLiteral() string {
	return b.token.Text
}

// TokenKind ...
func (b *BooleanExpression) TokenKind() token.Kind {
	return b.token.Kind
}

func (b *BooleanExpression) String() string {
	return b.value
}

// Value ...
func (b *BooleanExpression) Value() string {
	return b.value
}

// Type ...
func (b *BooleanExpression) Type() types.Type {
	return &base.Boolean{Name: "Boolean"}
}

func (b *BooleanExpression) exprNode() {}

// Attr ...
func (b *BooleanExpression) Attr(attr string) any {
	switch attr {
	case "type":
		return b.Type()
	default:
		return ""
	}
}
