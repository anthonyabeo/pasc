package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/symbols/dtype"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// FuncDeclaration is the node type for a function declaration in the AST
type FuncDeclaration struct {
	Token      token.Token
	Name       *Identifier
	Parameters []*Parameter
	ReturnType dtype.Type
	Block      *Block
}

// TokenLiteral returns the text value this node's token.
func (f *FuncDeclaration) TokenLiteral() string { return f.Token.Text }

// StatNode ...
func (f *FuncDeclaration) StatNode() string {
	return fmt.Sprintf("function(%v):%v", f.Parameters, f.ReturnType)
}

func (f *FuncDeclaration) String() string {
	return fmt.Sprintf("function(%v):%v", f.Parameters, f.ReturnType)
}
