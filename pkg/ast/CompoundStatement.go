package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// CompoundStatement is the node in the AST that represents a compound statement.
type CompoundStatement struct {
	Token      token.Token
	Statements []Statement
	Label      string
}

func (cs *CompoundStatement) Accept(vst Visitor) error {
	return vst.VisitCompoundStatement(cs)
}

func (cs *CompoundStatement) Pos() *token.Position {
	return cs.Token.Pos
}

func (cs *CompoundStatement) stmt() {}

func (cs *CompoundStatement) String() string {
	return fmt.Sprintf("%v", cs.Statements)
}

func (cs *CompoundStatement) SetLabel(l string) {
	cs.Label = l
}
