package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// NilValue represents node for nil values.
type NilValue struct {
	Token token.Token
}

// TokenLiteral returns the text value this node's token.
func (n *NilValue) TokenLiteral() string { return n.Token.Text }

func (n *NilValue) exprNode() {}

func (n *NilValue) String() string {
	return "nil"
}
