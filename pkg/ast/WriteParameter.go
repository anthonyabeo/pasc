package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// WriteParameter is the node that represents a single parameter to be passed to the wite(ln) procedure
type WriteParameter struct {
	E          Expression
	TotalWidth Expression
	FracDigits Expression
}

// TokenLiteral returns the text value this node's token.
func (w *WriteParameter) TokenLiteral() string { return "write-parameter" }

func (w *WriteParameter) exprNode() {}

// TokenKind ...
func (w *WriteParameter) TokenKind() token.Kind {
	return token.And
}

// Attr ...
func (w *WriteParameter) Attr(attr string) interface{} {
	return nil
}

func (w *WriteParameter) String() string {
	return fmt.Sprintf("%v:%v:%v", w.E, w.TotalWidth, w.FracDigits)
}

// RValue ...
func (w *WriteParameter) RValue() Expression { return nil }

// LValue ...
func (w *WriteParameter) LValue() Expression { return nil }
