package ast

import "fmt"

// ProcedureStatement models a procedure statement node
type ProcedureStatement struct {
	ProcedureID *Identifier
	ParamList   []Expression
}

// NewProcedureStatement creates and returns a new ProcedureStatement
func NewProcedureStatement(pID *Identifier) *ProcedureStatement {
	return &ProcedureStatement{ProcedureID: pID}
}

// TokenLiteral returns the text value this node's token.
func (ps *ProcedureStatement) TokenLiteral() string {
	return ps.ProcedureID.Token.Text
}

func (ps *ProcedureStatement) statNode() {}

func (ps *ProcedureStatement) String() string {
	return fmt.Sprintf("%v(%v)", ps.ProcedureID.Name, ps.ParamList)
}
