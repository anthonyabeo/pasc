package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// FuncDeclaration is the node type for a function declaration in the AST
type FuncDeclaration struct {
	Heading   *FuncHeading
	Block     *Block
	Directive *Identifier
	Label     string
}

func (f *FuncDeclaration) stmt() {}

func (f *FuncDeclaration) Accept(v Visitor) {
	v.VisitFuncDeclaration(f)
}

func (f *FuncDeclaration) String() string {
	return fmt.Sprintf("function(%v):%v", f.Heading.Parameters, f.Heading.ReturnType)
}

func (f *FuncDeclaration) SetLabel(l string) {
	f.Label = l
}

// FuncHeading denotes a function's signature.
type FuncHeading struct {
	TokenKind  token.Kind
	Name       *Identifier
	Parameters []FormalParameter
	ReturnType types.Type
	Lbl        string
}

func (f *FuncHeading) GetName() string {
	return f.Name.Name
}

func (f *FuncHeading) formalParam() {}

func (f *FuncHeading) String() string {
	var pList []string
	for _, p := range f.Parameters {
		pList = append(pList, p.String())
	}

	return fmt.Sprintf("function %s(%s):%s", f.Name.Name, strings.Join(pList, ", "), f.ReturnType.GetName())
}
