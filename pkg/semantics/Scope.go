package symbols

import (
	"fmt"
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
	w.EnterSymbol("integer", TYPE, &base.Integer{Name: "integer"})
	w.EnterSymbol("Boolean", TYPE, &base.Boolean{Name: "Boolean"})
	w.EnterSymbol("real", TYPE, &base.Real{Name: "real"})
	w.EnterSymbol("char", TYPE, &base.Char{Name: "char"})

	w.EnterSymbol("true", CONST, &base.Boolean{Name: "Boolean"})
	w.EnterSymbol("false", CONST, &base.Boolean{Name: "Boolean"})
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
func (w *WonkySymbolTable) EnterSymbol(name string, kind Kind, typ types.Type) {
	oldSym := w.RetrieveSymbol(name)
	if oldSym != nil && oldSym.Depth() == w.Depth {
		panic(fmt.Sprintf("symbol %s already declared", name))
	}

	var newSym Symbol
	switch kind {
	case VARIABLE:
		newSym = NewVariable(name, kind, typ)
	case CONST:
		newSym = NewConst(name, kind, typ)
	case FUNCTION:
	default:
		panic("invalid symbol kind")
	}

	// add to scope display
	newSym.SetDepth(w.Depth)
	newSym.SetVar(oldSym)
	w.ScopeDisplay[w.Depth] = append(w.ScopeDisplay[w.Depth], newSym)

	w.Table[newSym.Name()] = newSym
}

// RetrieveSymbol ...
func (w *WonkySymbolTable) RetrieveSymbol(name string) Symbol {
	return w.Table[name]
}

// DeclaredLocally ...
func (w *WonkySymbolTable) DeclaredLocally(name string) bool {
	sym := w.RetrieveSymbol(name)
	if sym == nil {
		panic(fmt.Sprintf("symbol %s not defined", name))
	}

	return sym.Depth() == w.Depth
}

//
//import (
//	"fmt"
//	"github.com/anthonyabeo/pasc/pkg/types/base"
//)
//
//// Scope denotes a generic scope. Any entity that has a scope implements this interface
//type Scope interface {
//	GetScopeName() string
//	GetEnclosingScope() Scope
//	Define(Symbol) error
//	Resolve(name string) Symbol
//}
//
//// LocalScope denotes a local scope, e.g. function scope
//type LocalScope struct {
//	Name    string
//	Symbols map[string]Symbol
//	Parent  Scope
//}
//
//// NewLocalScope creates and returns a new local scope
//func NewLocalScope(name string, parent Scope) *LocalScope {
//	return &LocalScope{
//		Symbols: make(map[string]Symbol),
//		Parent:  parent,
//		Name:    name,
//	}
//}
//
//// GetScopeName returns the name of the function
//func (l *LocalScope) GetScopeName() string {
//	return l.Name
//}
//
//// GetEnclosingScope returns this scope's parent scope
//func (l *LocalScope) GetEnclosingScope() Scope {
//	return l.Parent
//}
//
//// Define insert a new symbol into the current scope
//func (l *LocalScope) Define(sym Symbol) error {
//	if _, exists := l.Symbols[sym.GetName()]; exists {
//		return fmt.Errorf("symbol %v already defined", sym.GetName())
//	}
//
//	l.Symbols[sym.GetName()] = sym
//
//	return nil
//}
//
//// Resolve retrieve the symbol associated with 'name' argument.
//// If the symbol is not found in the current scope, Resolve will
//// recursively search the parent scopes.
//func (l *LocalScope) Resolve(name string) Symbol {
//	sym := l.Symbols[name]
//	if sym != nil {
//		return sym
//	}
//
//	if l.Parent != nil {
//		return l.Parent.Resolve(name)
//	}
//
//	return nil
//}
//
//// GlobalScope denotes global scope. There can only be one global scope
//type GlobalScope struct {
//	Name    string
//	Symbols map[string]Symbol
//	Parent  Scope
//}
//
//// NewGlobalScope creates and returns a new global scope
//func NewGlobalScope(parent Scope) *GlobalScope {
//	g := &GlobalScope{
//		Name:    "global",
//		Symbols: make(map[string]Symbol),
//		Parent:  parent,
//	}
//
//	g.initTypeSystem()
//	return g
//}
//
//func (g *GlobalScope) initTypeSystem() {
//	g.Define(NewInteger("integer", TYPE, &base.Integer{Name: "integer"}))
//	g.Define(NewBoolean("Boolean", TYPE, &base.Boolean{Name: "Boolean"}))
//	g.Define(NewReal("real", TYPE, &base.Real{Name: "real"}))
//	g.Define(NewChar("char", TYPE, &base.Char{Name: "char"}))
//
//	g.Define(NewConst("true", CONST, &base.Boolean{Name: "Boolean"}))
//	g.Define(NewConst("false", CONST, &base.Boolean{Name: "Boolean"}))
//}
//
//// GetScopeName returns the name of the function
//func (g *GlobalScope) GetScopeName() string {
//	return g.Name
//}
//
//// GetEnclosingScope returns this scope's parent scope
//func (g *GlobalScope) GetEnclosingScope() Scope {
//	return nil
//}
//
//// Define insert a new symbol into the current scope
//func (g *GlobalScope) Define(sym Symbol) error {
//	if val, exists := g.Symbols[sym.GetName()]; exists {
//		return fmt.Errorf(
//			"symbol '%v' already defined as type '%v'", sym.GetName(), val.GetType().GetName())
//	}
//
//	g.Symbols[sym.GetName()] = sym
//
//	return nil
//}
//
//// Resolve retrieve the symbol associated with 'name' argument.
//// If the symbol is not found in the current scope, Resolve will
//// recursively search the parent scope.
//func (g *GlobalScope) Resolve(name string) Symbol {
//	sym := g.Symbols[name]
//	if sym != nil {
//		return sym
//	}
//
//	if g.Parent != nil {
//		return g.Parent.Resolve(name)
//	}
//
//	return nil
//}
