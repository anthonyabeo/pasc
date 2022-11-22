package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// VarDecl models the variable definition node in the AST
type VarDecl struct {
	Token token.Token
	Names []*Identifier
	Type  Type
}

// TokenLiteral returns the text value this node's token.
func (v *VarDecl) TokenLiteral() string { return v.Token.Text }
