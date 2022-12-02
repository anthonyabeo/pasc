package ast

import "github.com/anthonyabeo/pasc/pkg/dtype"

// Parameter ...
type Parameter struct {
	Names []*Identifier
	Type  dtype.Type
}

// TokenLiteral returns the text value this node's token.
func (p *Parameter) TokenLiteral() string { return "parameter" }

func (p *Parameter) statNode() {}
