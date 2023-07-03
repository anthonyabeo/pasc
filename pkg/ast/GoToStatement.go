package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// GotoStatement ...
type GotoStatement struct {
	TokenKind token.Kind
	Label     *UIntegerLiteral
	Lbl       string
}

func (g *GotoStatement) Accept(v Visitor) {
	v.VisitGotoStatement(g)
}

// StatNode ...
func (g *GotoStatement) stmt() {}

func (g *GotoStatement) String() string {
	return fmt.Sprintf(`goto %v`, g.Label.String())
}

func (g *GotoStatement) SetLabel(l string) {
	g.Lbl = l
}
