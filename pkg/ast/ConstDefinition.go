package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// ConstDefinition models the constant-definition-part of a block
type ConstDefinition struct {
	Token  token.Token
	Consts []*ConstDef
}

// ConstDef models a single constant definition
type ConstDef struct {
	Name  *Identifier
	Value Expression
}
