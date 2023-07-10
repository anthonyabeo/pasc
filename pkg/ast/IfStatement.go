package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// IfStatement is the node in the AST that represents an If Statement
type IfStatement struct {
	TokenKind token.Kind
	BoolExpr  Expression
	TruePath  Statement
	ElsePath  Statement
	Label     string
}

func (f *IfStatement) Accept(vst Visitor) error {
	return vst.VisitIfStatement(f)
}

// StatNode ...
func (f *IfStatement) stmt() {}

func (f *IfStatement) String() string {
	return fmt.Sprintf("if(%v) then %v else %v", f.BoolExpr, f.TruePath, f.ElsePath)
}

func (f *IfStatement) SetLabel(l string) {
	f.Label = l
}
