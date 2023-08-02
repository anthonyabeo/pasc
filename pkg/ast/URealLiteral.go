package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// URealLiteral represents an unsigned floating point number
type URealLiteral struct {
	Token token.Token
	Value string
	EType types.Type
}

func (ur *URealLiteral) expr() {}

func (ur *URealLiteral) String() string {
	return ur.Value
}
func (ur *URealLiteral) Pos() *token.Position {
	return ur.Token.Pos
}

func (ur *URealLiteral) Accept(vst Visitor) error {
	return vst.VisitURealLiteral(ur)
}

func (ur *URealLiteral) Type() types.Type {
	return ur.EType
}
