package ast

import (
	"fmt"
)

// CompoundStatement is the node in the AST that represents a compound statement.
type CompoundStatement struct {
	Statements []Statement
	Label      string
}

func (cs *CompoundStatement) Accept(v Visitor) {
	v.VisitCompoundStatement(cs)
}

// StatNode ...
func (cs *CompoundStatement) stmt() {}

func (cs *CompoundStatement) String() string {
	return fmt.Sprintf("%v", cs.Statements)
}

func (cs *CompoundStatement) SetLabel(l string) {
	cs.Label = l
}
