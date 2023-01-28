package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// VarDeclaration models the variable definition node in the AST
type VarDeclaration struct {
	Token token.Token
	Decls []*VarDecl
}

// TokenLiteral returns the text value this node's token.
func (v *VarDeclaration) TokenLiteral() string { return v.Token.Text }

// TokenKind returns this node's token's kind
func (v *VarDeclaration) TokenKind() token.Kind { return v.Token.Kind }

// StatNode ...
func (v *VarDeclaration) StatNode() string {
	return fmt.Sprintf("%v", v.Decls)
}

func (v *VarDeclaration) String() string {
	return fmt.Sprintf("%v", v.Decls)
}

// Gen creates and emits BRIL code for this node
func (v *VarDeclaration) Gen() {}

// VarDecl ...
type VarDecl struct {
	Names []*Identifier
	Type  types.Type
	Scope symbols.Scope
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

// Gen creates and emits BRIL code for this node
func (v *VarDecl) Gen() {}
