package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// UnaryExpression ...
type UnaryExpression struct {
	Operator token.Token
	Operand  Expression
	EvalType types.Type
}

// TokenLiteral returns the text value this node's token.
func (u *UnaryExpression) TokenLiteral() string { return u.Operator.Text }

func (u *UnaryExpression) exprNode() {}

// TokenKind returns this node's token's kind
func (u *UnaryExpression) TokenKind() token.Kind {
	return u.Operator.Kind
}

// Attr ...
func (u *UnaryExpression) Attr(attr string) interface{} {
	switch attr {
	case "type":
		return u.EvalType
	default:
		return nil
	}
}

func (u *UnaryExpression) String() string {
	return fmt.Sprintf("%v%v", u.Operator.Text, u.Operand)
}

// RValue ...
func (u *UnaryExpression) RValue() Expression { return nil }

// LValue ...
func (u *UnaryExpression) LValue() Expression { return nil }
