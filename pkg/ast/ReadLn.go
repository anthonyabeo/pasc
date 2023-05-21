package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type ReadLn struct {
	Name      string
	File      *Identifier
	VarAccess []Expression
	Label     string
}

// TokenLiteral returns the text value this node's token.
func (r *ReadLn) TokenLiteral() string {
	return r.Name
}

// TokenKind returns this node's token's kind
func (r *ReadLn) TokenKind() token.Kind {
	return token.Identifier
}

// StatNode ...
func (r *ReadLn) StatNode() string {
	return fmt.Sprintf("%v(%v)", r.Name, r.VarAccess)
}

func (r *ReadLn) String() string {
	return fmt.Sprintf("%v(%v)", r.Name, r.VarAccess)
}

func (r *ReadLn) GetName() string {
	return r.Name
}

func (r *ReadLn) GetParamList() []Expression {
	return r.VarAccess
}

func (r *ReadLn) SetLabel(l string) {
	r.Label = l
}
