package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Field denotes a record field symbol
type Field struct {
	Name   string
	Kind   Kind
	Type   types.Type
	Offset string
}

// NewField creates and returns a new field symbol
func NewField(name string, kind Kind, typ types.Type, offset string) *Field {
	return &Field{Name: name, Kind: kind, Type: typ, Offset: offset}
}

// GetKind returns the kind of this symbol
func (f *Field) GetKind() Kind {
	return f.Kind
}

// GetName returns the name of this symbol
func (f *Field) GetName() string {
	return f.Name
}

// GetType returns the type of this symbol
func (f *Field) GetType() types.Type {
	return f.Type
}

func (f *Field) String() string {
	return f.Name
}
