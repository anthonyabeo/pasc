package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// FuncDeclaration is the node type for a function declaration in the AST
type FuncDeclaration struct {
	Token      token.Token
	Name       *Identifier
	Parameters []*Parameter
	ReturnType types.Type
	Block      *Block
	Scope      symbols.Scope
}

// TokenLiteral returns the text value this node's token.
func (f *FuncDeclaration) TokenLiteral() string { return f.Token.Text }

// TokenKind returns this node's token's kind
func (f *FuncDeclaration) TokenKind() token.Kind { return f.Token.Kind }

// StatNode ...
func (f *FuncDeclaration) StatNode() string {
	return fmt.Sprintf("function(%v):%v", f.Parameters, f.ReturnType)
}

func (f *FuncDeclaration) String() string {
	return fmt.Sprintf("function(%v):%v", f.Parameters, f.ReturnType)
}
