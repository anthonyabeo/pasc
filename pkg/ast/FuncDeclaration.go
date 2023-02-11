package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// FuncDeclaration is the node type for a function declaration in the AST
type FuncDeclaration struct {
	Heading   *FuncHeading
	Block     *Block
	Directive *Identifier
	Scope     symbols.Scope
}

// TokenLiteral returns the text value this node's token.
func (f *FuncDeclaration) TokenLiteral() string { return f.Heading.Token.Text }

// TokenKind returns this node's token's kind
func (f *FuncDeclaration) TokenKind() token.Kind { return f.Heading.Token.Kind }

// StatNode ...
func (f *FuncDeclaration) StatNode() string {
	return fmt.Sprintf("function(%v):%v", f.Heading.Parameters, f.Heading.ReturnType)
}

func (f *FuncDeclaration) String() string {
	return fmt.Sprintf("function(%v):%v", f.Heading.Parameters, f.Heading.ReturnType)
}

// FuncHeading denotes a function's signature.
type FuncHeading struct {
	Token      token.Token
	Name       *Identifier
	Parameters []FormalParameter
	ReturnType types.Type
}

func (f *FuncHeading) formalParam() {}

func (f *FuncHeading) String() string {
	var pList []string
	for _, p := range f.Parameters {
		pList = append(pList, p.String())
	}

	return fmt.Sprintf("function %s(%s):%s", f.Name.Name, pList, f.ReturnType.GetName())
}
