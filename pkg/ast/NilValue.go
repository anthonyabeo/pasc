package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// NilValue represents node for nil values.
type NilValue struct {
	Token token.Token
	EType types.Type
}

func (n *NilValue) Accept(vst Visitor) error {
	return vst.VisitNil(n)
}

func (n *NilValue) Pos() *token.Position {
	return n.Token.Pos
}

func (n *NilValue) Type() types.Type {
	return n.EType
}

func (n *NilValue) expr() {}

func (n *NilValue) String() string {
	return "nil"
}
