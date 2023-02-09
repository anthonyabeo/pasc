package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Boolean ...
type Boolean struct {
	Name string
	Kind Kind
	Type types.Type
}

// NewBooleanSymbol ...
func NewBooleanSymbol(name string, kind Kind, typ types.Type) *Boolean {
	return &Boolean{Name: name, Kind: kind, Type: typ}
}

// GetKind returns the kind of this symbol
func (b *Boolean) GetKind() Kind {
	return b.Kind
}

// GetName returns the name of this symbol
func (b *Boolean) GetName() string {
	return b.Name
}

// GetType ...
func (b *Boolean) GetType() types.Type {
	return b.Type
}

func (b *Boolean) String() string {
	return b.Name
}