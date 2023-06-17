package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// StrLiteral models the node for character string literals
type StrLiteral struct {
	Token    token.Token
	Value    string
	EvalType types.Type
}

// TokenLiteral returns the text value this node's token.
func (c *StrLiteral) TokenLiteral() string { return c.Token.Text }

func (c *StrLiteral) exprNode() {}

// TokenKind returns this node's token's kind
func (c *StrLiteral) TokenKind() token.Kind {
	return c.Token.Kind
}

// Attr ...
func (c *StrLiteral) Attr(attr string) any {
	switch attr {
	case "type":
		return c.EvalType
	default:
		return ""
	}
}

func (c *StrLiteral) String() string {
	return c.Value
}
