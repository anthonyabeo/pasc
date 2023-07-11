package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// BinaryExpression ...
type BinaryExpression struct {
	Left, Right Expression
	Operator    token.Kind
	EType       types.Type
}

func (b *BinaryExpression) expr() {}

func (b *BinaryExpression) Type() types.Type {
	return b.EType
}

func (b *BinaryExpression) Accept(vst Visitor) error {
	return vst.VisitBinaryExpr(b)
}

func (b *BinaryExpression) String() string {
	return fmt.Sprintf("%s %s %s", b.Left, b.Operator, b.Right)
}
