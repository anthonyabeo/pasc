package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// GotoStatement ...
type GotoStatement struct {
	Token token.Token
	Label *UIntegerLiteral
	Lbl   string
}

func (g *GotoStatement) Accept(vst Visitor) error {
	return vst.VisitGotoStatement(g)
}

func (g *GotoStatement) Pos() *token.Position {
	return g.Token.Pos
}

func (g *GotoStatement) stmt() {}

func (g *GotoStatement) String() string {
	return fmt.Sprintf(`goto %v`, g.Label.String())
}

func (g *GotoStatement) SetLabel(l string) {
	g.Lbl = l
}
