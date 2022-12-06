package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// UIntegerLiteral ...
type UIntegerLiteral struct {
	Token token.Token
	Value string
}

// NewUIntegerLiteral ...
func NewUIntegerLiteral(tt token.Token) *UIntegerLiteral {
	return &UIntegerLiteral{Token: tt, Value: tt.Text}
}

// TokenLiteral returns the text value this node's token.
func (u *UIntegerLiteral) TokenLiteral() string {
	return u.Token.Text
}

func (u *UIntegerLiteral) exprNode() {}

func (u *UIntegerLiteral) String() string {
	return u.Value
}
