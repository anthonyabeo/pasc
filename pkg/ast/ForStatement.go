package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
)

// ForStatement ...
type ForStatement struct {
	Token                 token.Token
	CtrlID                *Identifier
	InitValue, FinalValue Expression
	Body                  Statement
	Direction             token.Kind
	Label                 string
}

func (f *ForStatement) Accept(vst Visitor) error {
	return vst.VisitForStatement(f)
}

func (f *ForStatement) Pos() *token.Position {
	return f.Token.Pos
}

// StatNode ...
func (f *ForStatement) stmt() {}

func (f *ForStatement) String() string {
	return ""
}

func (f *ForStatement) SetLabel(l string) {
	f.Label = l
}
