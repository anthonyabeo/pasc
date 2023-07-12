package ast

import (
	"fmt"
	"strings"
)

// ProcedureStmt models a procedure statement node
type ProcedureStmt struct {
	Name      *Identifier
	ParamList []Expression
	Label     string
}

func (ps *ProcedureStmt) stmt() {}

func (ps *ProcedureStmt) Accept(vst Visitor) error {
	return vst.VisitProcedureStmt(ps)
}

func (ps *ProcedureStmt) String() string {
	var params []string
	for _, param := range ps.ParamList {
		params = append(params, param.String())
	}

	return fmt.Sprintf("%v(%v)", ps.Name.Name, strings.Join(params, ", "))
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
