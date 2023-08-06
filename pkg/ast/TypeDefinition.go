package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// TypeDefinition ...
type TypeDefinition struct {
	Token token.Token
	Types []*TypeDef
}

func (t *TypeDefinition) Accept(vst Visitor) error {
	return vst.VisitTypeDef(t)
}

func (t *TypeDefinition) decl() {}

func (t *TypeDefinition) String() string {
	var typeDefs []string
	for _, td := range t.Types {
		typeDefs = append(typeDefs, td.String(), "\n\t")
	}

	return fmt.Sprintf("type\n\t%v", typeDefs)
}

func (t *TypeDefinition) Pos() *token.Position {
	return t.Token.Pos
}

// TypeDef ...
type TypeDef struct {
	Name        *Identifier
	TypeDenoter types.Type
}

func (t *TypeDef) String() string {
	return fmt.Sprintf("%v = %v", t.Name.Name, t.TypeDenoter)
}
