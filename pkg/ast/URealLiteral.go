package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// URealLiteral represents an unsigned floating point number
type URealLiteral struct {
	Token token.Token
	Value string
}

// TokenLiteral returns the text value this node's token.
func (ur *URealLiteral) TokenLiteral() string {
	return ur.Token.Text
}

func (ur *URealLiteral) exprNode() {}

func (ur *URealLiteral) String() string {
	return ur.Value
}
