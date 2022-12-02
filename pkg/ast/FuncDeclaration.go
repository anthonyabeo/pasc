package ast

import (
	"github.com/anthonyabeo/pasc/pkg/dtype"
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

func (f *FuncDeclaration) statNode() {}
