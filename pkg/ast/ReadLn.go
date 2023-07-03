package ast

import (
	"fmt"
)

type ReadLn struct {
	Name      string
	File      *Identifier
	VarAccess []Expression
	Label     string
}

func (r *ReadLn) stmt() {}

func (r *ReadLn) Accept(v Visitor) {
	v.VisitReadLn(r)
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
