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

func (f *FuncDeclaration) Accept(vst Visitor) error {
	return vst.VisitFuncDeclaration(f)
}

func (f *FuncDeclaration) Pos() *token.Position {
	return f.Heading.Pos()
}

func (f *FuncDeclaration) String() string {
	return fmt.Sprintf("function(%v):%v", f.Heading.Parameters, f.Heading.ReturnType)
}

func (f *FuncDeclaration) SetLabel(l string) {
	f.Label = l
}

// FuncHeading denotes a function's signature.
type FuncHeading struct {
	Token      token.Token
	FName      *Identifier
	Parameters []FormalParameter
	ReturnType types.Type
	Lbl        string
}

func (f *FuncHeading) Name() string {
	var pList []string
	for _, p := range f.Parameters {
		pList = append(pList, p.String())
	}

	return fmt.Sprintf("function (%s):%s", strings.Join(pList, ", "), f.ReturnType)
}

func (f *FuncHeading) formalParam() {}

func (f *FuncHeading) expr() {}

func (f *FuncHeading) Type() types.Type {
	return f
}

func (f *FuncHeading) Underlying() types.Type {
	return f
}

func (f *FuncHeading) Pos() *token.Position {
	return f.Token.Pos
}

func (f *FuncHeading) Accept(vst Visitor) error {
	return vst.VisitFuncHeading(f)
}

func (f *FuncHeading) String() string {
	var pList []string
	for _, p := range f.Parameters {
		pList = append(pList, p.String())
	}

	return fmt.Sprintf("function %s(%s):%s", f.FName.Name, strings.Join(pList, ", "), f.ReturnType)
}
