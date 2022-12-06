package ast

import "fmt"

// ProgramAST defines the root node of the AST. It correlates to the
// start symbol in the grammer
type ProgramAST struct {
	Block *Block
}

// TokenLiteral returns the text value this node's token.
func (p *ProgramAST) TokenLiteral() string {
	return "program"
}

func (p *ProgramAST) String() string {
	return fmt.Sprintf("%v", p.Block)
}
