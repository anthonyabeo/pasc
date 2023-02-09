package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// URealLiteral represents an unsigned floating point number
type URealLiteral struct {
	Token    token.Token
	Value    string
	EvalType types.Type
}

// TokenLiteral returns the text value this node's token.
func (ur *URealLiteral) TokenLiteral() string {
	return ur.Token.Text
}

// TokenKind returns this node's token's kind
func (ur *URealLiteral) TokenKind() token.Kind {
	return ur.Token.Kind
}

func (ur *URealLiteral) exprNode() {}

func (ur *URealLiteral) String() string {
	return ur.Value
}

// Attr ...
func (ur *URealLiteral) Attr(attr string) any {
	switch attr {
	case "type":
		return ur.EvalType
	default:
		return ""
	}
}

// RValue ...
func (ur *URealLiteral) RValue() Expression { return nil }

// LValue ...
func (ur *URealLiteral) LValue() Expression { return nil }
