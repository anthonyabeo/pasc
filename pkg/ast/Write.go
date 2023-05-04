package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type Write struct {
	Name      string
	File      *Identifier
	ParamList []Expression
}

// TokenLiteral returns the text value this node's token.
func (w *Write) TokenLiteral() string {
	return w.Name
}

// TokenKind returns this node's token's kind
func (w *Write) TokenKind() token.Kind {
	return token.Identifier
}

// StatNode ...
func (w *Write) StatNode() string {
	return fmt.Sprintf("%v(%v)", w.Name, w.ParamList)
}

func (w *Write) String() string {
	return fmt.Sprintf("%v(%v)", w.Name, w.ParamList)
}

func (w *Write) GetName() string {
	return w.Name
}

func (w *Write) GetParamList() []Expression {
	return w.ParamList
}
