package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Identifier models an identifier node in the AST
type Identifier struct {
	Token token.Token
	Name  string
	EType types.Type
}

func (id *Identifier) expr() {}

func (id *Identifier) Accept(vst Visitor) error {
	return vst.VisitIdentifier(id)
}

func (id *Identifier) Type() types.Type {
	return id.EType
}

func (id *Identifier) Pos() *token.Position {
	return id.Token.Pos
}

func (id *Identifier) String() string {
	return id.Name
}
