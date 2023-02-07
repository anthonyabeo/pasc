package ast

import (
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Parameter ...
type Parameter struct {
	Names []*Identifier
	Type  types.Type
}
