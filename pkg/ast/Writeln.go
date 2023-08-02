package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type Writeln struct {
	Token     token.Token
	Name      string
	File      *Identifier
	ParamList []Expression
	Label     string
}

func (w *Writeln) stmt() {}

func (w *Writeln) Accept(vst Visitor) error {
	return vst.VisitWriteln(w)
}

func (w *Writeln) Pos() *token.Position {
	return w.Token.Pos
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
