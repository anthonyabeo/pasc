package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// SetConstructor ...
type SetConstructor struct {
	Token   token.Token
	Members []Expression
	EType   types.Type
}

func (s *SetConstructor) expr() {}

func (s *SetConstructor) Accept(v Visitor) {
	v.VisitSetConstructor(s)
}

func (s *SetConstructor) Type() types.Type {
	return s.EType
}

func (s *SetConstructor) String() string {
	return "nil"
}
