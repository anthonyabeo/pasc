package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type Read struct {
	Name      string
	File      *Identifier
	VarAccess []Expression
	Label     string
}

// TokenLiteral returns the text value this node's token.
func (r *Read) TokenLiteral() string {
	return r.Name
}

// TokenKind returns this node's token's kind
func (r *Read) TokenKind() token.Kind {
	return token.Identifier
}

// StatNode ...
func (r *Read) StatNode() string {
	return fmt.Sprintf("%v(%v)", r.Name, r.VarAccess)
}

func (r *Read) String() string {
	return fmt.Sprintf("%v(%v)", r.Name, r.VarAccess)
}

func (r *Read) GetName() string {
	return r.Name
}

func (r *Read) GetParamList() []Expression {
	return r.VarAccess
}

func (r *Read) SetLabel(l string) {
	r.Label = l
}
