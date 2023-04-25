package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// Program defines the root node of the AST. It correlates to the
// start symbol in the grammar
type Program struct {
	Token     token.Token
	Name      *Identifier
	ParamList []*Identifier
	Block     *Block
}

// TokenLiteral returns the text value this node's token.
func (p *Program) TokenLiteral() string {
	return fmt.Sprintf("program %v(%v)", p.Name.String(), p.ParamList)
}

// TokenKind returns this node's token's kind
func (p *Program) TokenKind() token.Kind { return p.Token.Kind }

func (p *Program) String() string {
	return fmt.Sprintf("program %v(%v)", p.Name.String(), p.ParamList)
}
