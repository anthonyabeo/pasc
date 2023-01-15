package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// BinaryExpression ...
type BinaryExpression struct {
	Left, Right Expression
	Operator    token.Token
	EvalType    types.Type
}

// TokenLiteral returns the text value this node's token.
func (b *BinaryExpression) TokenLiteral() string { return b.Operator.Text }

func (b *BinaryExpression) exprNode() {}

func (b *BinaryExpression) TokenKind() token.Kind {
	return b.Operator.Kind
}

func (b *BinaryExpression) Attr(attr string) interface{} {
	switch attr {
	case "type":
		return b.EvalType
	default:
		return nil
	}
}
