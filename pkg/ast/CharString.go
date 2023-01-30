package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
)

// CharString models the node for character string literals
type CharString struct {
	Token    token.Token
	Value    string
	EvalType string
}

// TokenLiteral returns the text value this node's token.
func (c *CharString) TokenLiteral() string { return c.Token.Text }

func (c *CharString) exprNode() {}

// TokenKind returns this node's token's kind
func (c *CharString) TokenKind() token.Kind {
	return c.Token.Kind
}

// Attr ...
func (c *CharString) Attr(attr string) string {
	switch attr {
	case "type":
		return c.EvalType
	default:
		return ""
	}
}

func (c *CharString) String() string {
	return c.Value
}

// RValue ...
func (c *CharString) RValue() Expression { return nil }

// LValue ...
func (c *CharString) LValue() Expression { return nil }
