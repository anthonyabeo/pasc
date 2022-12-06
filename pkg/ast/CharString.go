package ast

import "github.com/anthonyabeo/pasc/pkg/token"

type CharString struct {
	Token token.Token
	Value string
}

// TokenLiteral returns the text value this node's token.
func (c *CharString) TokenLiteral() string { return c.Token.Text }

func (c *CharString) exprNode() {}
