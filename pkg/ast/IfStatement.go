package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// IfStatement ...
type IfStatement struct {
	Token    token.Token
	BoolExpr Expression
	TruePath Statement
	ElsePath Statement
}

// TokenLiteral returns the text value this node's token.
func (f *IfStatement) TokenLiteral() string { return f.Token.Text }

func (f *IfStatement) statNode() {}
