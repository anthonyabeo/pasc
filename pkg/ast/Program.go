package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// ProgramAST defines the root node of the AST. It correlates to the
// start symbol in the grammer
type ProgramAST struct {
	Token     token.Token
	Name      *Identifier
	ParamList []*Identifier
	Block     *Block
}

// TokenLiteral returns the text value this node's token.
func (p *ProgramAST) TokenLiteral() string {
	return fmt.Sprintf("program %v(%v)", p.Name.String(), p.ParamList)
}

// TokenKind returns this node's token's kind
func (p *ProgramAST) TokenKind() token.Kind { return p.Token.Kind }

func (p *ProgramAST) String() string {
	return fmt.Sprintf("program %v(%v)", p.Name.String(), p.ParamList)
}
