package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Const denotes a constant symbol
type Const struct {
	Name string
	Kind Kind
	Type types.Type
}

// NewConstSymbol creates and returns a new constant symbol
func NewConstSymbol(name string, kind Kind, typ types.Type) *Const {
	return &Const{Name: name, Kind: kind, Type: typ}
}

// GetKind returns the kind of this symbol
func (c *Const) GetKind() Kind {
	return c.Kind
}

// GetName returns the name of this symbol
func (c *Const) GetName() string {
	return c.Name
}

// GetType ...
func (c *Const) GetType() types.Type {
	return c.Type
}

func (c *Const) String() string {
	return c.Name
}
