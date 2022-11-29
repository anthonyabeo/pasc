package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// Identifier models an identifier node in the AST
type Identifier struct {
	token token.Token
	name  string
}

// NewIdentifier creates and returns a new Identifier
func NewIdentifier(token token.Token, name string) *Identifier {
	return &Identifier{token: token, name: name}
}

// TokenLiteral returns the text value this node's token.
func (id *Identifier) TokenLiteral() string {
	return id.token.Text
}

func (id *Identifier) exprNode() {}
