package ast

// WriteParameter ...
type WriteParameter struct {
	E          Expression
	TotalWidth Expression
	FracDigits Expression
}

// TokenLiteral returns the text value this node's token.
func (w *WriteParameter) TokenLiteral() string { return "write-parameter" }

func (w *WriteParameter) exprNode() {}
