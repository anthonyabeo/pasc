package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// StrLiteral models the node for character string literals
type StrLiteral struct {
	Token token.Token
	Value string
	EType types.Type
}

func (str *StrLiteral) expr() {}

func (str *StrLiteral) Accept(vst Visitor) error {
	return vst.VisitStrLiteral(str)
}

func (str *StrLiteral) Pos() *token.Position {
	return str.Token.Pos
}

func (str *StrLiteral) Type() types.Type {
	return str.EType
}

func (str *StrLiteral) String() string {
	return str.Value
}
