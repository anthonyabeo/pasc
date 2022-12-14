package symbols

import "github.com/anthonyabeo/pasc/pkg/symbols/dtype"

// Variable denotes a variable symbol
type Variable struct {
	Name string
	Kind Kind
	Type dtype.Type
}

// NewVariableSymbol ...
func NewVariableSymbol(name string, kind Kind, typ dtype.Type) *Variable {
	return &Variable{Name: name, Kind: kind, Type: typ}
}

// GetKind returns the kind of this symbol
func (v *Variable) GetKind() Kind {
	return v.Kind
}

// GetName returns the name of this symbol
func (v *Variable) GetName() string {
	return v.Name
}

// GetType ...
func (v *Variable) GetType() dtype.Type {
	return v.Type
}

func (v *Variable) String() string {
	return v.Name
}
