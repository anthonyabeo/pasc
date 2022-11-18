package ast

// ProgramAST defines the root node of the AST. It correlates to the
// start symbol in the grammer
type ProgramAST struct {
	// label definition part
	// constant definition part
	// type definition part
	// variable declaration part
	// procedure and function declaration part
	Stats []Statement
}

// TokenLiteral returns the text value this node's token.
func (p *ProgramAST) TokenLiteral() string {
	return "program"
}
