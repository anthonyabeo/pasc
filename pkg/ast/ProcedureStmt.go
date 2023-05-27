package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// ProcedureStmt models a procedure statement node
type ProcedureStmt struct {
	Name      *Identifier
	ParamList []Expression
	Label     string
}

// TokenLiteral returns the text value this node's token.
func (ps *ProcedureStmt) TokenLiteral() string {
	return ps.Name.Token.Text
}

// TokenKind returns this node's token's kind
func (ps *ProcedureStmt) TokenKind() token.Kind {
	return ps.Name.Token.Kind
}

// StatNode ...
func (ps *ProcedureStmt) StatNode() string {
	return fmt.Sprintf("%v(%v)", ps.Name.Name, ps.ParamList)
}

func (ps *ProcedureStmt) String() string {
	return fmt.Sprintf("%v(%v)", ps.Name.Name, ps.ParamList)
}

func (ps *ProcedureStmt) GetName() string {
	return ps.Name.Name
}

func (ps *ProcedureStmt) GetParamList() []Expression {
	return ps.ParamList
}

func (ps *ProcedureStmt) SetLabel(l string) {
	ps.Label = l
}
