package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// UnaryExpression ...
type UnaryExpression struct {
	Operator token.Kind
	Operand  Expression
	EType    types.Type
}

func (u *UnaryExpression) expr() {}

func (u *UnaryExpression) Type() types.Type {
	return u.EType
}

func (u *UnaryExpression) Accept(vst Visitor) error {
	return vst.VisitUnaryExpr(u)
}

func (u *UnaryExpression) String() string {
	return fmt.Sprintf("%v %v", u.Operator, u.Operand)
}
