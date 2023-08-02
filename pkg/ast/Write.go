package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type Write struct {
	Token     token.Token
	Name      string
	File      *Identifier
	ParamList []Expression
	Label     string
}

func (w *Write) stmt() {}

func (w *Write) Accept(vst Visitor) error {
	return vst.VisitWrite(w)
}
func (w *Write) Pos() *token.Position {
	return w.Token.Pos
}

func (w *Write) String() string {
	return fmt.Sprintf("%v(%v)", w.Name, w.ParamList)
}

func (w *Write) GetName() string {
	return w.Name
}

func (w *Write) GetParamList() []Expression {
	return w.ParamList
}

func (w *Write) SetLabel(l string) {
	w.Label = l
}
