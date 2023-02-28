package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// TypeDefinition ...
type TypeDefinition struct {
	Token token.Token
	Types []*TypeDef
}

// TypeDef ...
type TypeDef struct {
	Name        *Identifier
	TypeDenoter types.Type
}
