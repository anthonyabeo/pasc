package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type Read struct {
	Token     token.Token
	Name      string
	File      *Identifier
	VarAccess []Expression
	Label     string
}

func (r *Read) stmt() {}

func (r *Read) Accept(vst Visitor) error {
	return vst.VisitRead(r)
}

func (r *Read) Pos() *token.Position {
	return r.Token.Pos
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
