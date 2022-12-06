package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// NilValue ...
type NilValue struct {
	Token token.Token
}

// TokenLiteral returns the text value this node's token.
func (n *NilValue) TokenLiteral() string { return n.Token.Text }

func (n *NilValue) exprNode() {}
