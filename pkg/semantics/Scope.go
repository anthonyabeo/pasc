package semantics

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types/base"
)

// SymbolTable ...
type SymbolTable interface {
	OpenScope()
	CloseScope()
	EnterSymbol(string, Symbol)
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
	w.EnterSymbol("integer", NewInteger("integer", TYPE, base.NewInteger()))
	w.EnterSymbol("Boolean", NewBoolean("Boolean", TYPE, base.NewBoolean()))
	w.EnterSymbol("real", NewReal("real", TYPE, base.NewReal()))
	w.EnterSymbol("char", NewChar("char", TYPE, base.NewChar()))

	w.EnterSymbol("true", NewConst("true", CONST, base.NewBoolean(), &ast.BoolLiteral{Value: "true"}))
	w.EnterSymbol("false", NewConst("false", CONST, base.NewBoolean(), &ast.BoolLiteral{Value: "false"}))
	w.EnterSymbol("maxint", NewConst("maxint", CONST, base.NewInteger(), &ast.UIntegerLiteral{
		Token: token.NewToken(token.UIntLiteral, "9223372036854775807", nil),
		Value: "9223372036854775807",
		EType: base.NewInteger(),
	}))
	w.EnterSymbol("abs", NewFunction("abs", FUNCTION, &ast.FuncHeading{
		Token: token.NewToken(token.Function, "function", nil),
		FName: &ast.Identifier{Token: token.NewToken(token.Identifier, "abs", nil), Name: "abs"},
		Parameters: []ast.FormalParameter{&ast.ValueParam{Names: []*ast.Identifier{
			{Token: token.NewToken(token.Identifier, "x", nil), Name: "x", EType: base.NewReal()}}, Typ: base.NewReal()}},
		ReturnType: base.NewReal(),
	}))
	w.EnterSymbol("sqr", NewFunction("sqr", FUNCTION, &ast.FuncHeading{
		Token: token.NewToken(token.Function, "function", nil),
		FName: &ast.Identifier{Token: token.NewToken(token.Identifier, "sqr", nil), Name: "sqr"},
		Parameters: []ast.FormalParameter{&ast.ValueParam{Names: []*ast.Identifier{
			{Token: token.NewToken(token.Identifier, "x", nil), Name: "x", EType: base.NewReal()}}, Typ: base.NewReal()}},
		ReturnType: base.NewReal(),
	}))
	w.EnterSymbol("sin", NewFunction("sin", FUNCTION, &ast.FuncHeading{
		Token: token.NewToken(token.Function, "function", nil),
		FName: &ast.Identifier{Token: token.NewToken(token.Identifier, "sin", nil), Name: "sin"},
		Parameters: []ast.FormalParameter{&ast.ValueParam{Names: []*ast.Identifier{
			{Token: token.NewToken(token.Identifier, "x", nil), Name: "x", EType: base.NewReal()}}, Typ: base.NewReal()}},
		ReturnType: base.NewReal(),
	}))
	w.EnterSymbol("cos", NewFunction("cos", FUNCTION, &ast.FuncHeading{
		Token: token.NewToken(token.Function, "function", nil),
		FName: &ast.Identifier{Token: token.NewToken(token.Identifier, "cos", nil), Name: "cos"},
		Parameters: []ast.FormalParameter{&ast.ValueParam{Names: []*ast.Identifier{
			{Token: token.NewToken(token.Identifier, "x", nil), Name: "x", EType: base.NewReal()}}, Typ: base.NewReal()}},
		ReturnType: base.NewReal(),
	}))
	w.EnterSymbol("exp", NewFunction("exp", FUNCTION, &ast.FuncHeading{
		Token: token.NewToken(token.Function, "function", nil),
		FName: &ast.Identifier{Token: token.NewToken(token.Identifier, "exp", nil), Name: "exp"},
		Parameters: []ast.FormalParameter{&ast.ValueParam{Names: []*ast.Identifier{
			{Token: token.NewToken(token.Identifier, "x", nil), Name: "x", EType: base.NewReal()}}, Typ: base.NewReal()}},
		ReturnType: base.NewReal(),
	}))
	w.EnterSymbol("ln", NewFunction("ln", FUNCTION, &ast.FuncHeading{
		Token: token.NewToken(token.Function, "function", nil),
		FName: &ast.Identifier{Token: token.NewToken(token.Identifier, "ln", nil), Name: "ln"},
		Parameters: []ast.FormalParameter{&ast.ValueParam{Names: []*ast.Identifier{
			{Token: token.NewToken(token.Identifier, "x", nil), Name: "x", EType: base.NewReal()}}, Typ: base.NewReal()}},
		ReturnType: base.NewReal(),
	}))
	w.EnterSymbol("sqrt", NewFunction("sqrt", FUNCTION, &ast.FuncHeading{
		Token: token.NewToken(token.Function, "function", nil),
		FName: &ast.Identifier{Token: token.NewToken(token.Identifier, "sqrt", nil), Name: "sqrt"},
		Parameters: []ast.FormalParameter{&ast.ValueParam{Names: []*ast.Identifier{
			{Token: token.NewToken(token.Identifier, "x", nil), Name: "x", EType: base.NewReal()}}, Typ: base.NewReal()}},
		ReturnType: base.NewReal(),
	}))
	w.EnterSymbol("arctan", NewFunction("arctan", FUNCTION, &ast.FuncHeading{
		Token: token.NewToken(token.Function, "function", nil),
		FName: &ast.Identifier{Token: token.NewToken(token.Identifier, "arctan", nil), Name: "arctan"},
		Parameters: []ast.FormalParameter{&ast.ValueParam{Names: []*ast.Identifier{
			{Token: token.NewToken(token.Identifier, "x", nil), Name: "x", EType: base.NewReal()}}, Typ: base.NewReal()}},
		ReturnType: base.NewReal(),
	}))
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
