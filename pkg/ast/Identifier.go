package ast

import (
	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Identifier models an identifier node in the AST
type Identifier struct {
	Token    token.Token
	Name     string
	Scope    symbols.Scope
	EvalType types.Type
}

// TokenLiteral returns the text value this node's token.
func (id *Identifier) TokenLiteral() string {
	return id.Token.Text
}

func (id *Identifier) exprNode() {}

func (id *Identifier) TokenKind() token.Kind {
	return id.Token.Kind
}

func (id *Identifier) Attr(attr string) interface{} {
	switch attr {
	case "type":
		return id.EvalType.GetName()
	default:
		return nil
	}
}

func (id *Identifier) String() string {
	return id.Name
}
