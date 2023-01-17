package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// CharString models the node for character string literals
type CharString struct {
	Token    token.Token
	Value    string
	EvalType types.Type
}

// TokenLiteral returns the text value this node's token.
func (c *CharString) TokenLiteral() string { return c.Token.Text }

func (c *CharString) exprNode() {}

// TokenKind ...
func (c *CharString) TokenKind() token.Kind {
	return c.Token.Kind
}

// Attr ...
func (c *CharString) Attr(attr string) interface{} {
	switch attr {
	case "type":
		return c.EvalType
	default:
		return nil
	}
}

func (c *CharString) String() string {
	return c.Value
}
