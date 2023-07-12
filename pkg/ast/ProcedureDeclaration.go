package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/types"
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
	return p.Heading.String()
}

func (p *ProcedureDeclaration) SetLabel(l string) {
	p.Label = l
}

// ProcedureHeading denotes a procedure's signature.
type ProcedureHeading struct {
	TokenKind  token.Kind
	PName      *Identifier
	Parameters []FormalParameter
}

func (p *ProcedureHeading) Name() string {
	var pList []string
	for _, param := range p.Parameters {
		pList = append(pList, param.String())
	}

	return fmt.Sprintf("procedure (%s)", strings.Join(pList, ", "))
}

func (p *ProcedureHeading) expr() {}

func (p *ProcedureHeading) Type() types.Type {
	return p
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

	return fmt.Sprintf("procedure %s(%s)", p.PName.Name, strings.Join(pList, ", "))
}
