package ast

import "fmt"

// WriteParameter is the node that represents a single parameter to be passed to the wite(ln) procedure
type WriteParameter struct {
	E          Expression
	TotalWidth Expression
	FracDigits Expression
}

// TokenLiteral returns the text value this node's token.
func (w *WriteParameter) TokenLiteral() string { return "write-parameter" }

func (w *WriteParameter) exprNode() {}

func (w *WriteParameter) String() string {
	return fmt.Sprintf("%v:%v:%v", w.E, w.TotalWidth, w.FracDigits)
}
