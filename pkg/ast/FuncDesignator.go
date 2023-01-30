package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// FuncDesignator is the node that represents a function call
type FuncDesignator struct {
	Name       *Identifier
	Parameters []Expression
	EvalType   string
	Scope      symbols.Scope
}

// TokenLiteral returns the text value this node's token.
func (f *FuncDesignator) TokenLiteral() string {
	// TODO change this to f.Token.Text
	return "function"
}

func (f *FuncDesignator) exprNode() {}

// TokenKind ...
func (f *FuncDesignator) TokenKind() token.Kind {
	return token.Function
}

// Attr ...
func (f *FuncDesignator) Attr(attr string) string {
	switch attr {
	case "type":
		return f.EvalType
	default:
		return ""
	}
}

func (f *FuncDesignator) String() string {
	return fmt.Sprintf("%v(%v)", f.Name, f.Parameters)
}

// RValue ...
func (f *FuncDesignator) RValue() Expression { return nil }

// LValue ...
func (f *FuncDesignator) LValue() Expression { return nil }
