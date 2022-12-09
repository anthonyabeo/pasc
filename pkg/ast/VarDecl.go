package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/dtype"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// VarDecl models the variable definition node in the AST
type VarDecl struct {
	Token token.Token
	Names []*Identifier
	Type  dtype.Type
}

// TokenLiteral returns the text value this node's token.
func (v *VarDecl) TokenLiteral() string { return v.Token.Text }

// StatNode ...
func (v *VarDecl) StatNode() string {
	return fmt.Sprintf("%v %v: %v", v.Token.Text, v.Names, v.Type.GetName())
}

func (v *VarDecl) String() string {
	return fmt.Sprintf("%v %v: %v", v.Token.Text, v.Names, v.Type.GetName())
}
