package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Identifier models an identifier node in the AST
type Identifier struct {
	TokenKind token.Kind
	Name      string
	EType     types.Type
}

func (id *Identifier) expr() {}

func (id *Identifier) Accept(v Visitor) {
	v.VisitIdentifier(id)
}

func (id *Identifier) Type() types.Type {
	return id.EType
}
func (id *Identifier) String() string {
	return id.Name
}
