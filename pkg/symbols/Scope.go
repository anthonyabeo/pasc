package symbols

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

// NewLocalScope ...
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
	l.Symbols[sym.getName()] = sym
}

// Resolve retrieve the symbol associated with 'name' argument.
// If the symbol is not found in the current scope, Resolve will
// recursively search the parent scope.
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
	Symbols map[string]Symbol
	Parent  Scope
}

// NewGlobalScope ...
func NewGlobalScope(parent Scope) *GlobalScope {
	return &GlobalScope{
		Symbols: make(map[string]Symbol),
		Parent:  parent,
	}
}

// GetScopeName returns the name of the function
func (g *GlobalScope) GetScopeName() string {
	return "global"
}

// GetEnclosingScope returns this scope's parent scope
func (g *GlobalScope) GetEnclosingScope() Scope {
	return nil
}

// Define insert a new symbol into the current scope
func (g *GlobalScope) Define(sym Symbol) {
	g.Symbols[sym.getName()] = sym
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
