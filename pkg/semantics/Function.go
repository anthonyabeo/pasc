package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Function denotes a function symbol
type Function struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewFunction creates and returns a new function symbol
func NewFunction(name string, kind Kind, typ types.Type) *Function {
	return &Function{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (f *Function) Kind() Kind {
	return f.kind
}

// Name returns the name of this symbol
func (f *Function) Name() string {
	return f.name
}

// Type returns the type of this symbol
func (f *Function) Type() types.Type {
	return f.typ
}

// Depth ...
func (f *Function) Depth() int {
	return f.depth
}

// Var ...
func (f *Function) Var() Symbol {
	return f.vr
}

// SetDepth ...
func (f *Function) SetDepth(d int) {
	f.depth = d
}

// SetVar ...
func (f *Function) SetVar(sym Symbol) {
	f.vr = sym
}

func (f *Function) String() string {
	return f.name
}
