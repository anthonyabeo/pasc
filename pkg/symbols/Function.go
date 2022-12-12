package symbols

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/symbols/dtype"
)

// Function denotes a variable symbol
type Function struct {
	Name        string
	Kind        Kind
	Type        dtype.Type
	symbols     map[string]Symbol
	parentScope Scope
}

// NewFunctionSymbol ...
func NewFunctionSymbol(name string, kind Kind, typ dtype.Type, parentScope Scope) *Function {
	return &Function{
		Name:        name,
		Kind:        kind,
		Type:        typ,
		symbols:     make(map[string]Symbol),
		parentScope: parentScope,
	}
}

func (f *Function) getKind() Kind {
	return f.Kind
}

func (f *Function) getName() string {
	return f.Name
}

func (f *Function) getType() dtype.Type {
	return f.Type
}

func (f *Function) String() string {
	if f.Type != nil {
		return fmt.Sprintf("<%v:%v>", f.Name, f.Type)
	}

	return f.Name
}

// GetScopeName returns the name of the function
func (f *Function) GetScopeName() string {
	return f.Name
}

// GetEnclosingScope returns this scope's parent scope
func (f *Function) GetEnclosingScope() Scope {
	return f.parentScope
}

// Define insert a new symbol into the current scope
func (f *Function) Define(sym Symbol) {
	f.symbols[sym.getName()] = sym
}

// Resolve retrieve the symbol associated with 'name' argument
func (f *Function) Resolve(name string) Symbol {
	if sym, found := f.symbols[name]; found {
		if sym != nil {
			return sym
		}

		return f.parentScope.Resolve(name)
	}

	return nil
}
