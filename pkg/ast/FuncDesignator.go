package ast

import "fmt"

// FuncDesignator is the node that represents a function call
type FuncDesignator struct {
	Name       *Identifier
	Parameters []Expression
}

// TokenLiteral returns the text value this node's token.
func (f *FuncDesignator) TokenLiteral() string { return "function-designator" }

func (f *FuncDesignator) exprNode() {}

func (f *FuncDesignator) String() string {
	return fmt.Sprintf("%v(%v)", f.Name, f.Parameters)
}
