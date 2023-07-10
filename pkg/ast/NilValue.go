package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// NilValue represents node for nil values.
type NilValue struct {
	TokenKind token.Kind
	EType     types.Type
}

func (n *NilValue) Accept(vst Visitor) error {
	return vst.VisitNil(n)
}

func (n *NilValue) Type() types.Type {
	return n.EType
}

func (n *NilValue) expr() {}

func (n *NilValue) String() string {
	return "nil"
}
