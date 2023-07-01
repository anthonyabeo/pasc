package semantics

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
)

// SymbolTable ...
type SymbolTable interface {
	OpenScope()
	CloseScope()
	EnterSymbol(string, Kind, types.Type)
	RetrieveSymbol(string) Symbol
	DeclaredLocally(string) bool
}

// WonkySymbolTable ...
type WonkySymbolTable struct {
	Depth        int
	Table        map[string]Symbol
	ScopeDisplay map[int][]Symbol
}

// NewWonkySymbolTable ...
func NewWonkySymbolTable() *WonkySymbolTable {
	w := &WonkySymbolTable{
		Depth:        0,
		Table:        make(map[string]Symbol),
		ScopeDisplay: make(map[int][]Symbol),
	}

	w.initSymbolTable()

	return w
}

func (w *WonkySymbolTable) initSymbolTable() {
	w.EnterSymbol("integer", NewInteger("integer", TYPE, &base.Integer{Name: "integer"}))
	w.EnterSymbol("Boolean", NewBoolean("Boolean", TYPE, &base.Boolean{Name: "Boolean"}))
	w.EnterSymbol("real", NewReal("real", TYPE, &base.Real{Name: "real"}))
	w.EnterSymbol("char", NewChar("char", TYPE, &base.Char{Name: "char"}))

	w.EnterSymbol("true", NewConst("true", CONST, &base.Boolean{Name: "Boolean"}, &ast.BoolLiteral{Value: "true"}))
	w.EnterSymbol("false", NewConst("false", CONST, &base.Boolean{Name: "Boolean"}, &ast.BoolLiteral{Value: "false"}))
}

// OpenScope ...
func (w *WonkySymbolTable) OpenScope() {
	w.Depth++
	w.ScopeDisplay[w.Depth] = make([]Symbol, 0)
}

// CloseScope ...
func (w *WonkySymbolTable) CloseScope() {
	for _, sym := range w.ScopeDisplay[w.Depth] {
		prevSym := sym.Var()
		delete(w.Table, sym.Name())
		if prevSym != nil {
			w.Table[prevSym.Name()] = prevSym
		}
	}

	delete(w.ScopeDisplay, w.Depth)
	w.Depth--
}

// EnterSymbol ...
func (w *WonkySymbolTable) EnterSymbol(name string, sym Symbol) {
	oldSym := w.RetrieveSymbol(name)
	if oldSym != nil && oldSym.Depth() == w.Depth {
		panic(fmt.Sprintf("symbol %s already declared", name))
	}

	// add to scope display
	sym.SetDepth(w.Depth)
	sym.SetVar(oldSym)
	w.ScopeDisplay[w.Depth] = append(w.ScopeDisplay[w.Depth], sym)

	w.Table[sym.Name()] = sym
}

// RetrieveSymbol ...
func (w *WonkySymbolTable) RetrieveSymbol(name string) Symbol {
	return w.Table[name]
}

// DeclaredLocally ...
func (w *WonkySymbolTable) DeclaredLocally(name string) bool {
	sym := w.RetrieveSymbol(name)
	if sym == nil {
		return false
	}

	return sym.Depth() == w.Depth
}
