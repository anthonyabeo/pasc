package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// FuncDesignator is the node that represents a function call
type FuncDesignator struct {
	Name       *Identifier
	Parameters []Expression
	EvalType   types.Type
}

// TokenLiteral returns the text value this node's token.
func (f *FuncDesignator) TokenLiteral() string { return "function-designator" }

func (f *FuncDesignator) exprNode() {}

func (f *FuncDesignator) TokenKind() token.Kind {
	return token.Function
}

func (f *FuncDesignator) Attr(attr string) interface{} {
	switch attr {
	case "type":
		return f.EvalType
	default:
		return nil
	}
}

func (f *FuncDesignator) String() string {
	return fmt.Sprintf("%v(%v)", f.Name, f.Parameters)
}
