package symbols

// Scope denotes a generic scope. Any entity that has a scope implements this interface
type Scope interface {
	GetScopeName() string
	GetEnclosingScope() Scope
	Define(Symbol)
	Resolve(name string) Symbol
}

// LocalScope denotes a local scope, e.g. function scope
type LocalScope interface {
	Scope
}

// GlobalScope denotes global scope. There can only be one global scope
type GlobalScope interface {
	Scope
}
