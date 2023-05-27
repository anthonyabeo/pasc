package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// WithStatement models the AST node of a With Statement
type WithStatement struct {
	Token         token.Token
	RecordVarList []Expression
	Body          Statement
	Label         string
}

// TokenLiteral returns the text value this node's token.
func (w *WithStatement) TokenLiteral() string { return w.Token.Text }

// TokenKind returns this node's token's kind
func (w *WithStatement) TokenKind() token.Kind { return w.Token.Kind }

// StatNode ...
func (w *WithStatement) StatNode() string {
	var recVarList []string
	for _, recVar := range w.RecordVarList {
		recVarList = append(recVarList, recVar.String())
	}

	return fmt.Sprintf("with %v do %v", strings.Join(recVarList, ", "), w.Body)
}

func (w *WithStatement) String() string {
	var recVarList []string
	for _, recVar := range w.RecordVarList {
		recVarList = append(recVarList, recVar.String())
	}

	return fmt.Sprintf("with %v do %v", strings.Join(recVarList, ", "), w.Body)
}

func (w *WithStatement) SetLabel(l string) {
	w.Label = l
}
