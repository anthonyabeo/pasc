package semantics

import (
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Const denotes a constant symbol
type Const struct {
	name  string
	kind  Kind
	typ   types.Type
	value ast.Expression

	depth int
	vr    Symbol
}

// NewConst creates and returns a new constant symbol
func NewConst(name string, kind Kind, typ types.Type, val ast.Expression) *Const {
	return &Const{name: name, kind: kind, typ: typ, value: val}
}

// Kind returns the kind of this symbol
func (c *Const) Kind() Kind {
	return c.kind
}

// Name returns the name of this symbol
func (c *Const) Name() string {
	return c.name
}

// Type returns the type of this symbol
func (c *Const) Type() types.Type {
	return c.typ
}

// Depth ...
func (c *Const) Depth() int {
	return c.depth
}

// Var ...
func (c *Const) Var() Symbol {
	return c.vr
}

// SetDepth ...
func (c *Const) SetDepth(d int) {
	c.depth = d
}

// SetVar ...
func (c *Const) SetVar(sym Symbol) {
	c.vr = sym
}

func (c *Const) String() string {
	return c.name
}
