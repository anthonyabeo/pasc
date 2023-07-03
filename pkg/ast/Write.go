package ast

import (
	"fmt"
)

type Write struct {
	Name      string
	File      *Identifier
	ParamList []Expression
	Label     string
}

func (w *Write) stmt() {}

func (w *Write) Accept(v Visitor) {
	v.VisitWrite(w)
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
