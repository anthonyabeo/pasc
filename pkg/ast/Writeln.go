package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type Writeln struct {
	Name      string
	File      *Identifier
	ParamList []Expression
	Label     string
}

// TokenLiteral returns the text value this node's token.
func (w *Writeln) TokenLiteral() string {
	return w.Name
}

// TokenKind returns this node's token's kind
func (w *Writeln) TokenKind() token.Kind {
	return token.Identifier
}

// StatNode ...
func (w *Writeln) StatNode() string {
	return fmt.Sprintf("%v(%v)", w.Name, w.ParamList)
}

func (w *Writeln) String() string {
	return fmt.Sprintf("%v(%v)", w.Name, w.ParamList)
}

func (w *Writeln) GetName() string {
	return w.Name
}

func (w *Writeln) GetParamList() []Expression {
	return w.ParamList
}

func (w *Writeln) SetLabel(l string) {
	w.Label = l
}
