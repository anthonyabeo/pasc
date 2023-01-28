package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

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

// TokenKind returns this node's token's kind
func (ps *ProcedureStatement) TokenKind() token.Kind {
	return ps.ProcedureID.Token.Kind
}

// StatNode ...
func (ps *ProcedureStatement) StatNode() string {
	return fmt.Sprintf("%v(%v)", ps.ProcedureID.Name, ps.ParamList)
}

func (ps *ProcedureStatement) String() string {
	return fmt.Sprintf("%v(%v)", ps.ProcedureID.Name, ps.ParamList)
}
