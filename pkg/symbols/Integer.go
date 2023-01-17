package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Integer ...
type Integer struct {
	Name string
	Kind Kind
	Type types.Type
}

// NewIntegerSymbol ...
func NewIntegerSymbol(name string, kind Kind, typ types.Type) *Integer {
	return &Integer{Name: name, Kind: kind, Type: typ}
}

// GetKind returns the kind of this symbol
func (i *Integer) GetKind() Kind {
	return i.Kind
}

// GetName returns the name of this symbol
func (i *Integer) GetName() string {
	return i.Name
}

// GetType ...
func (i *Integer) GetType() types.Type {
	return i.Type
}

func (i *Integer) String() string {
	return i.Name
}
