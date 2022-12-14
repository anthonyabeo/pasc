package symbols

import "github.com/anthonyabeo/pasc/pkg/symbols/dtype"

// Function denotes a variable symbol
type Function struct {
	Name  string
	Kind  Kind
	Type  dtype.Type
	Scope Scope
}

// NewFunctionSymbol creates and returns a new function symbol
func NewFunctionSymbol(name string, kind Kind, scope Scope) *Function {
	return &Function{
		Name:  name,
		Kind:  kind,
		Scope: scope,
	}
}

// GetKind returns the kind of this symbol
func (f *Function) GetKind() Kind {
	return f.Kind
}

// GetName returns the name of this symbol
func (f *Function) GetName() string {
	return f.Name
}

// GetType ...
func (f *Function) GetType() dtype.Type {
	return f.Type
}

func (f *Function) String() string {
	return f.Name
}
