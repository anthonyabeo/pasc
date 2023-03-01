package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Range ...
type Range struct {
	Token      token.Token
	Start, End Expression
	EvalType   types.Type
}

// TokenLiteral returns the text value this node's token.
func (r *Range) TokenLiteral() string { return r.Token.Text }

func (r *Range) exprNode() {}

// TokenKind ...
func (r *Range) TokenKind() token.Kind {
	return r.Token.Kind
}

// Attr ...
func (r *Range) Attr(attr string) any {
	switch attr {
	case "type":
		return r.EvalType
	default:
		return ""
	}
}

func (r *Range) String() string {
	return "range"
}

// RValue ...
func (r *Range) RValue() Expression { return nil }

// LValue ...
func (r *Range) LValue() Expression { return nil }
