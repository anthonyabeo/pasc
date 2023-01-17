package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Kind denotes the category of the symbol. e.g. variable, function
type Kind byte

const (
	VARIABLE Kind = iota
	FUNCTION
	PROCEDURE
	TYPE
)

// Symbol denotes a generic symbol
type Symbol interface {
	GetName() string
	GetKind() Kind
	GetType() types.Type
}
