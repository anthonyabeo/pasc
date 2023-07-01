package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Variable denotes a variable symbol
type Variable struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewVariable returns a new variable symbol type
func NewVariable(name string, kind Kind, typ types.Type) *Variable {
	return &Variable{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (v *Variable) Kind() Kind {
	return v.kind
}

// Name returns the name of this symbol
func (v *Variable) Name() string {
	return v.name
}

// Type returns the type of this symbol
func (v *Variable) Type() types.Type {
	return v.typ
}

// Depth ...
func (v *Variable) Depth() int {
	return v.depth
}

// Var ...
func (v *Variable) Var() Symbol {
	return v.vr
}

// SetDepth ...
func (v *Variable) SetDepth(d int) {
	v.depth = d
}

// SetVar ...
func (v *Variable) SetVar(sym Symbol) {
	v.vr = sym
}

func (v *Variable) String() string {
	return v.name
}
