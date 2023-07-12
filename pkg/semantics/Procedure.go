package semantics

import "github.com/anthonyabeo/pasc/pkg/types"

// Procedure denotes a procedure symbol
type Procedure struct {
	name string
	kind Kind
	typ  types.Type

	depth int
	vr    Symbol
}

// NewProcedure creates and returns a new procedure symbol
func NewProcedure(name string, kind Kind, typ types.Type) *Function {
	return &Function{name: name, kind: kind, typ: typ}
}

// Kind returns the kind of this symbol
func (p *Procedure) Kind() Kind {
	return p.kind
}

// Name returns the name of this symbol
func (p *Procedure) Name() string {
	return p.name
}

// Type returns the type of this symbol
func (p *Procedure) Type() types.Type {
	return p.typ
}

// Depth ...
func (p *Procedure) Depth() int {
	return p.depth
}

// Var ...
func (p *Procedure) Var() Symbol {
	return p.vr
}

// SetDepth ...
func (p *Procedure) SetDepth(d int) {
	p.depth = d
}

// SetVar ...
func (p *Procedure) SetVar(sym Symbol) {
	p.vr = sym
}

func (p *Procedure) String() string {
	return p.name
}
