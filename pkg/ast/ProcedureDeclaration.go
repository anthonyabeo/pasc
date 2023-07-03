package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// ProcedureDeclaration denotes a procedure declaration
type ProcedureDeclaration struct {
	Heading   *ProcedureHeading
	Block     *Block
	Directive *Identifier
	Label     string
}

func (p *ProcedureDeclaration) Accept(v Visitor) {
	v.VisitProcedureDecl(p)
}
func (p *ProcedureDeclaration) stmt() {}

func (p *ProcedureDeclaration) String() string {
	return fmt.Sprintf("%s()", p.Heading.Name)
}

func (p *ProcedureDeclaration) SetLabel(l string) {
	p.Label = l
}

// ProcedureHeading denotes a procedure's signature.
type ProcedureHeading struct {
	TokenKind  token.Kind
	Name       *Identifier
	Parameters []FormalParameter
}

func (f *ProcedureHeading) formalParam() {}

func (f *ProcedureHeading) String() string {
	var pList []string
	for _, p := range f.Parameters {
		pList = append(pList, p.String())
	}

	return fmt.Sprintf("function %s(%s)", f.Name.Name, strings.Join(pList, ", "))
}
