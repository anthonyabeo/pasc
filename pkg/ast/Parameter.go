package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// FormalParameter denotes a generic formal parameter type
type FormalParameter interface {
	formalParam()
	String() string
}

// ValueParam denoted a value parameter specification
type ValueParam struct {
	Names []*Identifier
	Type  types.Type
}

func (v *ValueParam) formalParam() {}

func (v *ValueParam) String() string {
	var vList []string
	for _, id := range v.Names {
		vList = append(vList, id.Name)
	}

	return fmt.Sprintf("%v: %v", strings.Join(vList, ", "), v.Type)
}

// VariableParam denotes a variable parameter specification
type VariableParam struct {
	Token token.Kind
	Names []*Identifier
	Type  types.Type
}

func (v *VariableParam) formalParam() {}

func (v *VariableParam) String() string {
	var vList []string
	for _, id := range v.Names {
		vList = append(vList, id.Name)
	}

	return fmt.Sprintf("var %v: %v", strings.Join(vList, ", "), v.Type)
}
