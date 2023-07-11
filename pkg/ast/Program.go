package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// Program defines the root node of the AST. It correlates to the
// start symbol in the grammar
type Program struct {
	TokenKind token.Kind
	Name      *Identifier
	ParamList []*Identifier
	Block     *Block
}

func (p *Program) String() string {
	return fmt.Sprintf("program %v(%v)", p.Name.String(), p.ParamList)
}
