package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Range ...
type Range struct {
	Token      token.Token
	Start, End Expression
	EType      types.Type
}

func (r *Range) Accept(v Visitor) {
	v.VisitRange(r)
}

func (r *Range) Type() types.Type {
	return r.EType
}

func (r *Range) expr() {}

func (r *Range) String() string {
	return "range"
}
