package ast

import (
	"fmt"

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

// TokenKind returns this node's token's kind
func (b *BinaryExpression) TokenKind() token.Kind {
	return b.Operator.Kind
}

// Attr ...
func (b *BinaryExpression) Attr(attr string) interface{} {
	switch attr {
	case "type":
		return b.EvalType.GetName()
	default:
		return nil
	}
}

func (b *BinaryExpression) String() string {
	return fmt.Sprintf("%s %s %s", b.Left, b.Operator.Text, b.Right)
}

// RValue ...
func (b *BinaryExpression) RValue() Expression { return nil }

// LValue ...
func (b *BinaryExpression) LValue() Expression { return nil }
