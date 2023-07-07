package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Boolean ...
type Boolean struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewBoolean creates and returns a new real-type symbol
func NewBoolean(name string, kind Kind, typ types.Type) *Boolean {
	return &Boolean{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (b *Boolean) Kind() Kind {
	return b.kind
}

// Name returns the name of this symbol
func (b *Boolean) Name() string {
	return b.name
}

// Type returns the type of this symbol
func (b *Boolean) Type() types.Type {
	return b.typ
}

// Depth ...
func (b *Boolean) Depth() int {
	return b.depth
}

// Var ...
func (b *Boolean) Var() Symbol {
	return b.vr
}

// SetDepth ...
func (b *Boolean) SetDepth(d int) {
	b.depth = d
}

// SetVar ...
func (b *Boolean) SetVar(sym Symbol) {
	b.vr = sym
}

func (b *Boolean) String() string {
	return b.name
}
