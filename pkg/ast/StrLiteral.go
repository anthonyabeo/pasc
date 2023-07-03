package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// StrLiteral models the node for character string literals
type StrLiteral struct {
	TokenKind token.Kind
	Value     string
	EType     types.Type
}

func (c *StrLiteral) expr() {}

func (c *StrLiteral) Accept(v Visitor) {
	v.VisitStrLiteral(c)
}

func (c *StrLiteral) Type() types.Type {
	return c.EType
}

func (c *StrLiteral) String() string {
	return c.Value
}
