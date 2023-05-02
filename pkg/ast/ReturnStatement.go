package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type ReturnStatement struct {
	Token token.Token
	Expr  Expression
}

// TokenLiteral returns the text value this node's token.
func (r *ReturnStatement) TokenLiteral() string {
	return r.Token.Text
}

// TokenKind returns this node's token kind
func (r *ReturnStatement) TokenKind() token.Kind {
	return r.Token.Kind
}

// StatNode ...
func (r *ReturnStatement) StatNode() string {
	return fmt.Sprintf("return %v", r.Expr)
}

func (r *ReturnStatement) String() string {
	return r.StatNode()
}
