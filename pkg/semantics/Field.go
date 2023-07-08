package semantics

import (
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/structured"
)

// Field denotes a record field symbol
type Field struct {
	name string
	kind Kind
	typ  types.Type

	depth  int
	vr     Symbol
	Rec    *structured.Record
	Offset uint64
}

// NewField creates and returns a new field symbol
func NewField(name string, kind Kind, typ types.Type, rec *structured.Record, offset uint64) *Field {
	return &Field{name: name, kind: kind, typ: typ, Rec: rec, Offset: offset}
}

// Kind returns the kind of this symbol
func (f *Field) Kind() Kind {
	return f.kind
}

// Name returns the name of this symbol
func (f *Field) Name() string {
	return f.name
}

// Type returns the type of this symbol
func (f *Field) Type() types.Type {
	return f.typ
}

// Depth ...
func (f *Field) Depth() int {
	return f.depth
}

// Var ...
func (f *Field) Var() Symbol {
	return f.vr
}

// SetDepth ...
func (f *Field) SetDepth(d int) {
	f.depth = d
}

// SetVar ...
func (f *Field) SetVar(sym Symbol) {
	f.vr = sym
}

func (f *Field) String() string {
	return f.name
}
