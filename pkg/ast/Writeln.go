package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type Writeln struct {
	Name      string
	File      *Identifier
	ParamList []Expression
}

// TokenLiteral returns the text value this node's token.
func (ps *Writeln) TokenLiteral() string {
	return ps.Name
}

// TokenKind returns this node's token's kind
func (ps *Writeln) TokenKind() token.Kind {
	return token.Identifier
}

// StatNode ...
func (ps *Writeln) StatNode() string {
	return fmt.Sprintf("%v(%v)", ps.Name, ps.ParamList)
}

func (ps *Writeln) String() string {
	return fmt.Sprintf("%v(%v)", ps.Name, ps.ParamList)
}

func (ps *Writeln) GetName() string {
	return ps.Name
}

func (ps *Writeln) GetParamList() []Expression {
	return ps.ParamList
}
