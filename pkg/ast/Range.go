package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Range ...
type Range struct {
	Token      token.Token
	Start, End Expression
	EType      types.Type
}

func (r *Range) Accept(vst Visitor) error {
	return vst.VisitRange(r)
}

func (r *Range) Pos() *token.Position {
	return r.Token.Pos
}

func (r *Range) Type() types.Type {
	return r.EType
}

func (r *Range) expr() {}

func (r *Range) String() string {
	return fmt.Sprintf("%s..%s", r.Start, r.End)
}
