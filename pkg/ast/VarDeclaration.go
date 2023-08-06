package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// VarDeclaration models the variable definition node in the AST
type VarDeclaration struct {
	Token token.Token
	Decls []*VarDecl
	Label string
}

func (vr *VarDeclaration) Accept(vst Visitor) error {
	return vst.VisitVarDecl(vr)
}

func (vr *VarDeclaration) Pos() *token.Position {
	return vr.Token.Pos
}

func (vr *VarDeclaration) decl() {}

func (vr *VarDeclaration) String() string {
	var decls []string
	for _, vd := range vr.Decls {
		decls = append(decls, vd.String())
	}

	return fmt.Sprintf("var\n\t%v", decls)
}

// VarDecl ...
type VarDecl struct {
	Names []*Identifier
	Type  types.Type
}

func (v *VarDecl) String() string {
	var names []string
	for _, name := range v.Names {
		names = append(names, name.Name)
	}

	return fmt.Sprintf("%v: %v", strings.Join(names, ", "), v.Type)
}
