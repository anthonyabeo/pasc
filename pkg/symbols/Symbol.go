package symbols

import "github.com/anthonyabeo/pasc/pkg/symbols/dtype"

// Kind denotes the category of the symbol. e.g. variable, function
type Kind byte

const (
	VARIABLE Kind = iota
	FUNCTION
	PROCEDURE
	BUILTIN_TYPE
)

// Symbol denotes a generic symbol
type Symbol interface {
	GetName() string
	GetKind() Kind
	GetType() dtype.Type
}
