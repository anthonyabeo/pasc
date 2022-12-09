package ast

import "fmt"

// ProgramAST defines the root node of the AST. It correlates to the
// start symbol in the grammer
type ProgramAST struct {
	Name      *Identifier
	ParamList []*Identifier
	Block     *Block
}

// TokenLiteral returns the text value this node's token.
func (p *ProgramAST) TokenLiteral() string {
	return fmt.Sprintf("program %v(%v)", p.Name.String(), p.ParamList)
}

func (p *ProgramAST) String() string {
	return fmt.Sprintf("program %v(%v)", p.Name.String(), p.ParamList)
}
