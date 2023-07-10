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

func (p *ProcedureDeclaration) Accept(vst Visitor) error {
	return vst.VisitProcedureDecl(p)
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

func (p *ProcedureHeading) formalParam() {}

func (p *ProcedureHeading) Accept(vst Visitor) error {
	return vst.VisitProcedureHeading(p)
}

func (p *ProcedureHeading) String() string {
	var pList []string
	for _, param := range p.Parameters {
		pList = append(pList, param.String())
	}

	return fmt.Sprintf("function %s(%s)", p.Name.Name, strings.Join(pList, ", "))
}
