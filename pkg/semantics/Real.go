package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Real ...
type Real struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewReal creates and returns a new real-type symbol
func NewReal(name string, kind Kind, typ types.Type) *Real {
	return &Real{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (r *Real) Kind() Kind {
	return r.kind
}

// Name returns the name of this symbol
func (r *Real) Name() string {
	return r.name
}

// Type returns the type of this symbol
func (r *Real) Type() types.Type {
	return r.typ
}

// Depth ...
func (r *Real) Depth() int {
	return r.depth
}

// Var ...
func (r *Real) Var() Symbol {
	return r.vr
}

// SetDepth ...
func (r *Real) SetDepth(d int) {
	r.depth = d
}

// SetVar ...
func (r *Real) SetVar(sym Symbol) {
	r.vr = sym
}

func (r *Real) String() string {
	return r.name
}
