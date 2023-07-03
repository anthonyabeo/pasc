package ast

import (
	"fmt"
)

type Read struct {
	Name      string
	File      *Identifier
	VarAccess []Expression
	Label     string
}

func (r *Read) stmt() {}

func (r *Read) Accept(v Visitor) {
	v.VisitRead(r)
}

func (r *Read) String() string {
	return fmt.Sprintf("%v(%v)", r.Name, r.VarAccess)
}

func (r *Read) GetName() string {
	return r.Name
}

func (r *Read) GetParamList() []Expression {
	return r.VarAccess
}

func (r *Read) SetLabel(l string) {
	r.Label = l
}
