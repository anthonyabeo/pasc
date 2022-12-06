package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// UIntegerLiteral is the node for an unsigned integer node in the AST
type UIntegerLiteral struct {
	Token token.Token
	Value string
}

// TokenLiteral returns the text value this node's token.
func (u *UIntegerLiteral) TokenLiteral() string {
	return u.Token.Text
}

func (u *UIntegerLiteral) exprNode() {}

func (u *UIntegerLiteral) String() string {
	return u.Value
}
