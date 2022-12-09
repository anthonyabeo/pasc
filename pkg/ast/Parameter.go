package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/dtype"
)

// Parameter ...
type Parameter struct {
	Names []*Identifier
	Type  dtype.Type
}

// TokenLiteral returns the text value this node's token.
func (p *Parameter) TokenLiteral() string { return "parameter" }

// StatNode ...
func (p *Parameter) StatNode() string {
	return fmt.Sprintf("%v:%v", p.Names, p.Type.GetName())
}
