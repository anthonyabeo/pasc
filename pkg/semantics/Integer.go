package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Integer ...
type Integer struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewInteger creates and returns a new integer-type symbol
func NewInteger(name string, kind Kind, typ types.Type) *Integer {
	return &Integer{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (i *Integer) Kind() Kind {
	return i.kind
}

// Name returns the name of this symbol
func (i *Integer) Name() string {
	return i.name
}

// Type returns the type of this symbol
func (i *Integer) Type() types.Type {
	return i.typ
}

// Depth ...
func (i *Integer) Depth() int {
	return i.depth
}

// Var ...
func (i *Integer) Var() Symbol {
	return i.vr
}

// SetDepth ...
func (i *Integer) SetDepth(d int) {
	i.depth = d
}

// SetVar ...
func (i *Integer) SetVar(sym Symbol) {
	i.vr = sym
}

func (i *Integer) String() string {
	return i.name
}
