package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// ConstDefinition models the constant-definition-part of a block
type ConstDefinition struct {
	Token  token.Token
	Consts []*ConstDef
}

func (c *ConstDefinition) Accept(vst Visitor) error {
	return vst.VisitConstDef(c)
}

func (c *ConstDefinition) String() string {
	var consts []string
	for _, cd := range c.Consts {
		consts = append(consts, cd.String())
	}

	return fmt.Sprintf("const\n\t%v", consts)
}

func (c *ConstDefinition) decl() {}

func (c *ConstDefinition) Pos() *token.Position {
	return c.Token.Pos
}

// ConstDef models a single constant definition
type ConstDef struct {
	Name  *Identifier
	Value Expression
}

func (c *ConstDef) String() string {
	return fmt.Sprintf("%v = %v", c.Name.Name, c.Value)
}
