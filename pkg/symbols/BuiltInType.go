package symbols

import "github.com/anthonyabeo/pasc/pkg/symbols/dtype"

// BuiltInType denotes a variable symbol
type BuiltInType struct {
	Name string
	Kind Kind
	Type dtype.Type
}

// GetKind returns the kind of this symbol
func (b *BuiltInType) GetKind() Kind {
	return b.Kind
}

// GetName returns the name of this symbol
func (b *BuiltInType) GetName() string {
	return b.Name
}

// GetType ...
func (b *BuiltInType) GetType() dtype.Type {
	return b.Type
}

func (b *BuiltInType) String() string {
	return b.Name
}
