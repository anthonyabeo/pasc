package ast

import (
	"fmt"
)

// ProcedureStmt models a procedure statement node
type ProcedureStmt struct {
	Name      *Identifier
	ParamList []Expression
	Label     string
}

func (ps *ProcedureStmt) stmt() {}

func (ps *ProcedureStmt) Accept(v Visitor) {
	v.VisitProcedureStmt(ps)
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
