package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// GotoStatement ...
type GotoStatement struct {
	Token token.Token
	Label *UIntegerLiteral
}

// TokenLiteral returns the text value this node's token.
func (g *GotoStatement) TokenLiteral() string { return g.Token.Text }

// TokenKind returns this node's token's kind
func (g *GotoStatement) TokenKind() token.Kind { return g.Token.Kind }

// StatNode ...
func (g *GotoStatement) StatNode() string {
	return fmt.Sprintf(`goto %v`, g.Label.String())
}

func (g *GotoStatement) String() string {
	return fmt.Sprintf(`goto %v`, g.Label.String())
}
