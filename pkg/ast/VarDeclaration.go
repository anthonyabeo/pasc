package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// VarDeclaration models the variable definition node in the AST
type VarDeclaration struct {
	Token token.Token
	// Names []*Identifier
	// Type  types.Type
	Decls []*VarDecl
}

// TokenLiteral returns the text value this node's token.
func (v *VarDeclaration) TokenLiteral() string { return v.Token.Text }

// StatNode ...
func (v *VarDeclaration) StatNode() string {
	return fmt.Sprintf("%v", v.Decls)
}

func (v *VarDeclaration) String() string {
	return fmt.Sprintf("%v", v.Decls)
}

// VarDecl ...
type VarDecl struct {
	Names []*Identifier
	Type  types.Type
}

// TokenLiteral returns the text value this node's token.
func (v *VarDecl) TokenLiteral() string { return "var" }

// StatNode ...
func (v *VarDecl) StatNode() string {
	return fmt.Sprintf("%v: %v", v.Names, v.Type.GetName())
}

func (v *VarDecl) String() string {
	return fmt.Sprintf("%v: %v", v.Names, v.Type.GetName())
}
