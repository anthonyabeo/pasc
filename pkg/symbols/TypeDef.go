package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// TypeDef ...
type TypeDef struct {
	Name string
	Kind Kind
	Type types.Type
}

// NewTypeDefSymbol ...
func NewTypeDefSymbol(name string, kind Kind, typ types.Type) *TypeDef {
	return &TypeDef{Name: name, Kind: kind, Type: typ}
}

// GetKind returns the kind of this symbol
func (t *TypeDef) GetKind() Kind {
	return t.Kind
}

// GetName returns the name of this symbol
func (t *TypeDef) GetName() string {
	return t.Name
}

// GetType ...
func (t *TypeDef) GetType() types.Type {
	return t.Type
}

func (t *TypeDef) String() string {
	return t.Name
}
