package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// SetConstructor ...
type SetConstructor struct {
	Token    token.Token
	Members  []Expression
	EvalType types.Type
}

// TokenLiteral returns the text value this node's token.
func (s *SetConstructor) TokenLiteral() string { return s.Token.Text }

func (s *SetConstructor) exprNode() {}

// TokenKind ...
func (s *SetConstructor) TokenKind() token.Kind {
	return s.Token.Kind
}

// Attr ...
func (s *SetConstructor) Attr(attr string) any {
	switch attr {
	case "type":
		return s.EvalType
	default:
		return ""
	}
}

func (s *SetConstructor) String() string {
	return "nil"
}
