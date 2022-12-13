package symbols

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
	getName() string
	getKind() Kind
}
