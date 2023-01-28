package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// UIntegerLiteral is the node for an unsigned integer node in the AST
type UIntegerLiteral struct {
	Token    token.Token
	Value    string
	EvalType types.Type
}

// TokenLiteral returns the text value this node's token.
func (u *UIntegerLiteral) TokenLiteral() string {
	return u.Token.Text
}

func (u *UIntegerLiteral) exprNode() {}

// TokenKind returns this node's token's kind
func (u *UIntegerLiteral) TokenKind() token.Kind {
	return u.Token.Kind
}

// Attr ...
func (u *UIntegerLiteral) Attr(attr string) interface{} {
	switch attr {
	case "type":
		return u.EvalType.GetName()
	default:
		return nil
	}
}

func (u *UIntegerLiteral) String() string {
	return u.Value
}

// RValue ...
func (u *UIntegerLiteral) RValue() Expression { return nil }

// LValue ...
func (u *UIntegerLiteral) LValue() Expression { return nil }
