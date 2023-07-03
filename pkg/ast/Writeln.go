package ast

import (
	"fmt"
)

type Writeln struct {
	Name      string
	File      *Identifier
	ParamList []Expression
	Label     string
}

func (w *Writeln) stmt() {}

func (w *Writeln) Accept(v Visitor) {
	v.VisitWriteln(w)
}

func (w *Writeln) String() string {
	return fmt.Sprintf("%v(%v)", w.Name, w.ParamList)
}

func (w *Writeln) GetName() string {
	return w.Name
}

func (w *Writeln) GetParamList() []Expression {
	return w.ParamList
}

func (w *Writeln) SetLabel(l string) {
	w.Label = l
}
