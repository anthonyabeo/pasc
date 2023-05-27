package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// WhileStatement models the AST node of a While Statement
type WhileStatement struct {
	Token    token.Token
	BoolExpr Expression
	Body     Statement
	Label    string
}

// TokenLiteral returns the text value this node's token.
func (w *WhileStatement) TokenLiteral() string { return w.Token.Text }

// TokenKind returns this node's token's kind
func (w *WhileStatement) TokenKind() token.Kind { return w.Token.Kind }

// StatNode ...
func (w *WhileStatement) StatNode() string {
	return fmt.Sprintf("while(%v) do %v", w.BoolExpr, w.Body)
}

func (w *WhileStatement) String() string {
	return fmt.Sprintf("while(%v) do %v", w.BoolExpr, w.Body)
}

func (w *WhileStatement) SetLabel(l string) {
	w.Label = l
}
