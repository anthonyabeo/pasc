package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"strings"
)

// ProcedureStmt models a procedure statement node
type ProcedureStmt struct {
	Name  *Identifier
	Args  []Expression
	Label string
}

func (ps *ProcedureStmt) stmt() {}

func (ps *ProcedureStmt) Accept(vst Visitor) error {
	return vst.VisitProcedureStmt(ps)
}

func (ps *ProcedureStmt) Pos() *token.Position {
	return ps.Name.Pos()
}

func (ps *ProcedureStmt) String() string {
	var params []string
	for _, param := range ps.Args {
		params = append(params, param.String())
	}

	return fmt.Sprintf("%v(%v)", ps.Name.Name, strings.Join(params, ", "))
}

func (ps *ProcedureStmt) GetName() string {
	return ps.Name.Name
}

func (ps *ProcedureStmt) GetParamList() []Expression {
	return ps.Args
}

func (ps *ProcedureStmt) SetLabel(l string) {
	ps.Label = l
}
