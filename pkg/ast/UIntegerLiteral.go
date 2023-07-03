package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// UIntegerLiteral is the node for an unsigned integer node in the AST
type UIntegerLiteral struct {
	TokenKind token.Kind
	Value     string
	EType     types.Type
}

func (u *UIntegerLiteral) expr() {}

func (u *UIntegerLiteral) Type() types.Type {
	return u.EType
}

func (u *UIntegerLiteral) Accept(v Visitor) {
	v.VisitUIntLiteral(u)
}

func (u *UIntegerLiteral) String() string {
	return u.Value
}
