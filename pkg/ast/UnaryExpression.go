package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// UnaryExpression ...
type UnaryExpression struct {
	Operator token.Token
	Operand  Expression
}

// TokenLiteral returns the text value this node's token.
func (u *UnaryExpression) TokenLiteral() string { return u.Operator.Text }

func (u *UnaryExpression) exprNode() {}
