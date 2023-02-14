package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Real ...
type Real struct {
	Name string
	Kind Kind
	Type types.Type
}

// NewRealSymbol ...
func NewRealSymbol(name string, kind Kind, typ types.Type) *Real {
	return &Real{Name: name, Kind: kind, Type: typ}
}

// GetKind returns the kind of this symbol
func (r *Real) GetKind() Kind {
	return r.Kind
}

// GetName returns the name of this symbol
func (r *Real) GetName() string {
	return r.Name
}

// GetType ...
func (r *Real) GetType() types.Type {
	return r.Type
}

func (r *Real) String() string {
	return r.Name
}
