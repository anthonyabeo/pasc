package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// Identifier models an identifier node in the AST
type Identifier struct {
	Token token.Token
	Name  string
}

// TokenLiteral returns the text value this node's token.
func (id *Identifier) TokenLiteral() string {
	return id.Token.Text
}

func (id *Identifier) exprNode() {}

func (id *Identifier) String() string {
	return id.Name
}
