package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Label denotes a label symbol
type Label struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewLabel returns a new label symbol
func NewLabel(name string, kind Kind, typ types.Type) *Label {
	return &Label{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (l *Label) Kind() Kind {
	return l.kind
}

// Name returns the name of this symbol
func (l *Label) Name() string {
	return l.name
}

// Type returns the type of this symbol
func (l *Label) Type() types.Type {
	return l.typ
}

// Depth ...
func (l *Label) Depth() int {
	return l.depth
}

// Var ...
func (l *Label) Var() Symbol {
	return l.vr
}

// SetDepth ...
func (l *Label) SetDepth(d int) {
	l.depth = d
}

// SetVar ...
func (l *Label) SetVar(sym Symbol) {
	l.vr = sym
}

func (l *Label) String() string {
	return l.name
}
