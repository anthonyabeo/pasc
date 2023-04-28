package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Variable denotes a variable symbol
type Variable struct {
	Name string
	Kind Kind
	Type types.Type
}

// NewVariable returns a new variable symbol type
func NewVariable(name string, kind Kind, typ types.Type) *Variable {
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

// GetType returns the type of this symbol
func (v *Variable) GetType() types.Type {
	return v.Type
}

func (v *Variable) String() string {
	return v.Name
}
