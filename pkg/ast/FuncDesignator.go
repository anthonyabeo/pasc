package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/types"
)

// FuncDesignator is the node that represents a function call
type FuncDesignator struct {
	Name       *Identifier
	Parameters []Expression
	EType      types.Type
	//Scope      semantics_tests.Scope
}

func (f *FuncDesignator) Type() types.Type {
	return f.EType
}
func (f *FuncDesignator) Accept(v Visitor) {
	v.VisitFuncDesignator(f)
}

func (f *FuncDesignator) expr() {}

func (f *FuncDesignator) String() string {
	return fmt.Sprintf("%v(%v)", f.Name, f.Parameters)
}
