package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// UIntegerLiteral is the node for an unsigned integer node in the AST
type UIntegerLiteral struct {
	Token token.Token
	Value string
	EType types.Type
}

func (u *UIntegerLiteral) expr() {}

func (u *UIntegerLiteral) Type() types.Type {
	return u.EType
}

func (u *UIntegerLiteral) Accept(vst Visitor) error {
	return vst.VisitUIntLiteral(u)
}

func (u *UIntegerLiteral) Pos() *token.Position {
	return u.Token.Pos
}

func (u *UIntegerLiteral) String() string {
	return u.Value
}
