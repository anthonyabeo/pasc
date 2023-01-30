package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
)

// NilValue represents node for nil values.
type NilValue struct {
	Token    token.Token
	EvalType string
}

// TokenLiteral returns the text value this node's token.
func (n *NilValue) TokenLiteral() string { return n.Token.Text }

func (n *NilValue) exprNode() {}

// TokenKind ...
func (n *NilValue) TokenKind() token.Kind {
	return n.Token.Kind
}

// Attr ...
func (n *NilValue) Attr(attr string) string {
	switch attr {
	case "type":
		return n.EvalType
	default:
		return ""
	}
}

func (n *NilValue) String() string {
	return "nil"
}

// RValue ...
func (n *NilValue) RValue() Expression { return nil }

// LValue ...
func (n *NilValue) LValue() Expression { return nil }
