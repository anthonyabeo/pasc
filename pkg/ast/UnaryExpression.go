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
	EType    types.Type
}

func (u *UnaryExpression) expr() {}

func (u *UnaryExpression) Type() types.Type {
	return u.EType
}

func (u *UnaryExpression) Accept(v Visitor) {
	v.VisitUnaryExpr(u)
}

func (u *UnaryExpression) String() string {
	return fmt.Sprintf("%v %v", u.Operator.Text, u.Operand)
}
