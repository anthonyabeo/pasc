package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Char ...
type Char struct {
	Name string
	Kind Kind
	Type types.Type
}

// NewCharSymbol ...
func NewCharSymbol(name string, kind Kind, typ types.Type) *Char {
	return &Char{Name: name, Kind: kind, Type: typ}
}

// GetKind returns the kind of this symbol
func (c *Char) GetKind() Kind {
	return c.Kind
}

// GetName returns the name of this symbol
func (c *Char) GetName() string {
	return c.Name
}

// GetType ...
func (c *Char) GetType() types.Type {
	return c.Type
}

func (c *Char) String() string {
	return c.Name
}
