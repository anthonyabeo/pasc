package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Char ...
type Char struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewChar creates and returns a new integer-type symbol
func NewChar(name string, kind Kind, typ types.Type) *Char {
	return &Char{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (c *Char) Kind() Kind {
	return c.kind
}

// Name returns the name of this symbol
func (c *Char) Name() string {
	return c.name
}

// Type returns the type of this symbol
func (c *Char) Type() types.Type {
	return c.typ
}

// Depth ...
func (c *Char) Depth() int {
	return c.depth
}

// Var ...
func (c *Char) Var() Symbol {
	return c.vr
}

// SetDepth ...
func (c *Char) SetDepth(d int) {
	c.depth = d
}

// SetVar ...
func (c *Char) SetVar(sym Symbol) {
	c.vr = sym
}

func (c *Char) String() string {
	return c.name
}
