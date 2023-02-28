package symbols

import "github.com/anthonyabeo/pasc/pkg/types/base"

// Scope denotes a generic scope. Any entity that has a scope implements this interface
type Scope interface {
	GetScopeName() string
	GetEnclosingScope() Scope
	Define(Symbol)
	Resolve(name string) Symbol
}

// LocalScope denotes a local scope, e.g. function scope
type LocalScope struct {
	Name    string
	Symbols map[string]Symbol
	Parent  Scope
}

// NewLocalScope creates and returns a new local scope
func NewLocalScope(name string, parent Scope) *LocalScope {
	return &LocalScope{
		Symbols: make(map[string]Symbol),
		Parent:  parent,
		Name:    name,
	}
}

// GetScopeName returns the name of the function
func (l *LocalScope) GetScopeName() string {
	return l.Name
}

// GetEnclosingScope returns this scope's parent scope
func (l *LocalScope) GetEnclosingScope() Scope {
	return l.Parent
}

// Define insert a new symbol into the current scope
func (l *LocalScope) Define(sym Symbol) {
	l.Symbols[sym.GetName()] = sym
}

// Resolve retrieve the symbol associated with 'name' argument.
// If the symbol is not found in the current scope, Resolve will
// recursively search the parent scopes.
func (l *LocalScope) Resolve(name string) Symbol {
	sym := l.Symbols[name]
	if sym != nil {
		return sym
	}

	if l.Parent != nil {
		return l.Parent.Resolve(name)
	}

	return nil
}

// GlobalScope denotes global scope. There can only be one global scope
type GlobalScope struct {
	Name    string
	Symbols map[string]Symbol
	Parent  Scope
}

// NewGlobalScope creates and returns a new global scope
func NewGlobalScope(parent Scope) *GlobalScope {
	g := &GlobalScope{
		Name:    "global",
		Symbols: make(map[string]Symbol),
		Parent:  parent,
	}

	g.initTypeSystem()
	return g
}

func (g *GlobalScope) initTypeSystem() {
	g.Define(NewIntegerSymbol("integer", TYPE, &base.Integer{Name: "integer"}))
	g.Define(NewBooleanSymbol("Boolean", TYPE, &base.Boolean{Name: "Boolean"}))
	g.Define(NewRealSymbol("real", TYPE, &base.Real{Name: "real"}))
	g.Define(NewCharSymbol("char", TYPE, &base.Char{Name: "char"}))
}

// GetScopeName returns the name of the function
func (g *GlobalScope) GetScopeName() string {
	return g.Name
}

// GetEnclosingScope returns this scope's parent scope
func (g *GlobalScope) GetEnclosingScope() Scope {
	return nil
}

// Define insert a new symbol into the current scope
func (g *GlobalScope) Define(sym Symbol) {
	g.Symbols[sym.GetName()] = sym
}

// Resolve retrieve the symbol associated with 'name' argument.
// If the symbol is not found in the current scope, Resolve will
// recursively search the parent scope.
func (g *GlobalScope) Resolve(name string) Symbol {
	sym := g.Symbols[name]
	if sym != nil {
		return sym
	}

	if g.Parent != nil {
		return g.Parent.Resolve(name)
	}

	return nil
}
