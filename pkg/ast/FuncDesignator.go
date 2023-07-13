package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/types"
)

// FuncDesignator is the node that represents a function call
type FuncDesignator struct {
	Name  *Identifier
	Args  []Expression
	EType types.Type
}

func (f *FuncDesignator) Type() types.Type {
	return f.EType
}
func (f *FuncDesignator) Accept(vst Visitor) error {
	return vst.VisitFuncDesignator(f)
}

func (f *FuncDesignator) expr() {}

func (f *FuncDesignator) String() string {
	var args []string
	for _, arg := range f.Args {
		args = append(args, arg.String())
	}

	return fmt.Sprintf("%v(%v)", f.Name, strings.Join(args, ", "))
}
