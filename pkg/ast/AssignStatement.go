package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// AssignStatement is the node used to represent Pascal assignments in the AST
type AssignStatement struct {
	Token    token.Token
	Variable Expression
	Value    Expression
	Label    string
}

// Accept ...
func (as *AssignStatement) Accept(vst Visitor) error {
	return vst.VisitAssignStmt(as)
}

func (as *AssignStatement) stmt() {}

func (as *AssignStatement) String() string {
	return fmt.Sprintf("%v := %v", as.Variable, as.Value)
}

func (as *AssignStatement) Pos() *token.Position {
	return as.Token.Pos
}

func (as *AssignStatement) SetLabel(l string) {
	as.Label = l
}
