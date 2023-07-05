package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// VarDeclaration models the variable definition node in the AST
type VarDeclaration struct {
	TokenKind token.Kind
	Decls     []*VarDecl
	Label     string
}

func (vr *VarDeclaration) String() string {
	return fmt.Sprintf("%v", vr.Decls)
}

// VarDecl ...
type VarDecl struct {
	Names []*Identifier
	Type  types.Type
}

func (v *VarDecl) String() string {
	return fmt.Sprintf("%v: %v", v.Names, v.Type)
}
