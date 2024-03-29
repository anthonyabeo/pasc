package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// WhileStatement models the AST node of a While Statement
type WhileStatement struct {
	Token    token.Token
	BoolExpr Expression
	Body     Statement
	Label    string
}

func (w *WhileStatement) Accept(vst Visitor) error {
	return vst.VisitWhileStatement(w)
}

func (w *WhileStatement) Pos() *token.Position {
	return w.Token.Pos
}

func (w *WhileStatement) stmt() {}

func (w *WhileStatement) String() string {
	return fmt.Sprintf("while(%v) do %v", w.BoolExpr, w.Body)
}

func (w *WhileStatement) SetLabel(l string) {
	w.Label = l
}
