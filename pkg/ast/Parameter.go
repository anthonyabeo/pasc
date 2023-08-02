package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// FormalParameter denotes a generic formal parameter type
type FormalParameter interface {
	Expression
	formalParam()
}

// ValueParam denoted a value parameter specification
type ValueParam struct {
	Names []*Identifier
	Typ   types.Type
}

func (v *ValueParam) formalParam() {}

func (v *ValueParam) Pos() *token.Position {
	return v.Names[0].Pos()
}

func (v *ValueParam) expr() {}

func (v *ValueParam) Type() types.Type {
	return v.Typ
}

func (v *ValueParam) Name() string {
	var vList []string
	for _, id := range v.Names {
		vList = append(vList, id.Name)
	}

	return fmt.Sprintf("%v: %v", strings.Join(vList, ", "), v.Typ)
}

func (v *ValueParam) Accept(vst Visitor) error {
	return vst.VisitValueParam(v)
}

func (v *ValueParam) String() string {
	var vList []string
	for _, id := range v.Names {
		vList = append(vList, id.Name)
	}

	return fmt.Sprintf("%v: %v", strings.Join(vList, ", "), v.Typ)
}

// VariableParam denotes a variable parameter specification
type VariableParam struct {
	Token token.Token
	Names []*Identifier
	Typ   types.Type
}

func (v *VariableParam) formalParam() {}

func (v *VariableParam) Pos() *token.Position {
	return v.Token.Pos
}

func (v *VariableParam) expr() {}

func (v *VariableParam) Type() types.Type {
	return v.Typ
}

func (v *VariableParam) Name() string {
	var vList []string
	for _, id := range v.Names {
		vList = append(vList, id.Name)
	}

	return fmt.Sprintf("var %v: %v", strings.Join(vList, ", "), v.Typ)
}

func (v *VariableParam) Accept(vst Visitor) error {
	return vst.VisitVariableParam(v)
}

func (v *VariableParam) String() string {
	var vList []string
	for _, id := range v.Names {
		vList = append(vList, id.Name)
	}

	return fmt.Sprintf("var %v: %v", strings.Join(vList, ", "), v.Typ)
}
