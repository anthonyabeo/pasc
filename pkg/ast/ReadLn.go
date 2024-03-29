package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type ReadLn struct {
	Token     token.Token
	Name      string
	File      *Identifier
	VarAccess []Expression
	Label     string
}

func (r *ReadLn) stmt() {}

func (r *ReadLn) Accept(vst Visitor) error {
	return vst.VisitReadLn(r)
}

func (r *ReadLn) Pos() *token.Position {
	return r.Token.Pos
}

func (r *ReadLn) String() string {
	return fmt.Sprintf("%v(%v)", r.Name, r.VarAccess)
}

func (r *ReadLn) GetName() string {
	return r.Name
}

func (r *ReadLn) GetParamList() []Expression {
	return r.VarAccess
}

func (r *ReadLn) SetLabel(l string) {
	r.Label = l
}
