package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Kind denotes the category of the symbol. e.g. variable, function
type Kind byte

const (
	VARIABLE Kind = iota
	FUNCTION
	PROCEDURE
	TYPE
	CONST
	LABEL
	FIELD
)

// Symbol denotes a generic symbol
type Symbol interface {
	Name() string
	Kind() Kind
	Type() types.Type

	Depth() int
	Var() Symbol
	SetVar(Symbol)
	SetDepth(int)
}
