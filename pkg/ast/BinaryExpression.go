package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// BinaryExpression ...
type BinaryExpression struct {
	Left, Right Expression
	Operator    token.Token
}

// TokenLiteral returns the text value this node's token.
func (b *BinaryExpression) TokenLiteral() string { return b.Operator.Text }

func (b *BinaryExpression) exprNode() {}
