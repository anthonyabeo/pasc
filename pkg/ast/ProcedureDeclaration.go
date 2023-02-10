package ast 

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

type ProcedureDeclaration struct {
	Heading *ProcedureHeading
	Block      *Block
	Directive *Identifier
}

// TokenLiteral returns the text value this node's token.
func (p *ProcedureDeclaration) TokenLiteral() string { return p.Heading.Token.Text }

// TokenKind returns this node's token's kind
func (p *ProcedureDeclaration) TokenKind() token.Kind { return p.Heading.Token.Kind }

// StatNode ...
func (p *ProcedureDeclaration) StatNode() string {
	return fmt.Sprintf("%s()", p.Heading.Name)
}

func (p *ProcedureDeclaration) String() string {
	return fmt.Sprintf("%s()", p.Heading.Name)
}


type ProcedureHeading struct {
	Token token.Token
	Name       *Identifier
	Parameters []*Parameter
}