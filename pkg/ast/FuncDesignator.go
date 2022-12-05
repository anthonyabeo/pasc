package ast

// FuncDesignator ...
type FuncDesignator struct {
	Name       *Identifier
	Parameters []Expression
}

// TokenLiteral returns the text value this node's token.
func (f *FuncDesignator) TokenLiteral() string { return "function-designator" }

func (f *FuncDesignator) exprNode() {}
