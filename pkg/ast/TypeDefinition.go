package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

type TypeDefinition struct {
	Token token.Token
	Types []*TypeDef
}

type TypeDef struct {
	Name        *Identifier
	TypeDenoter types.Type
}
