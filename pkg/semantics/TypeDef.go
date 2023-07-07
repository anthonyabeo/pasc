package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// TypeDef ...
type TypeDef struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewTypeDef returns a new type definition symbol
func NewTypeDef(name string, kind Kind, typ types.Type) *TypeDef {
	return &TypeDef{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (t *TypeDef) Kind() Kind {
	return t.kind
}

// Name returns the name of this symbol
func (t *TypeDef) Name() string {
	return t.name
}

// Type ...
func (t *TypeDef) Type() types.Type {
	return t.typ
}

// Depth ...
func (t *TypeDef) Depth() int {
	return t.depth
}

// Var ...
func (t *TypeDef) Var() Symbol {
	return t.vr
}

// SetDepth ...
func (t *TypeDef) SetDepth(d int) {
	t.depth = d
}

// SetVar ...
func (t *TypeDef) SetVar(sym Symbol) {
	t.vr = sym
}

func (t *TypeDef) String() string {
	return t.name
}
