package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
)

// ForStatement ...
type ForStatement struct {
	TokenKind             token.Kind
	CtrlID                *Identifier
	InitValue, FinalValue Expression
	Body                  Statement
	Direction             token.Kind
	Label                 string
}

func (f *ForStatement) Accept(vst Visitor) error {
	return vst.VisitForStatement(f)
}

// StatNode ...
func (f *ForStatement) stmt() {}

func (f *ForStatement) String() string {
	return ""
}

func (f *ForStatement) SetLabel(l string) {
	f.Label = l
}
