package codegen

import (
	"github.com/llir/llvm/ir"
)

// LLVMScope ...
type LLVMScope interface {
	GetScopeName() string
	GetEnclosingScope() LLVMScope
	Define(string, *ir.InstAlloca)
	Resolve(name string) *ir.InstAlloca
}

// SymbolTable ...
type SymbolTable struct {
	Name    string
	Symbols map[string]*ir.InstAlloca
	Parent  LLVMScope
}

// NewSymbolTable creates and returns a new symbol table
func NewSymbolTable(name string, parent LLVMScope) *SymbolTable {
	return &SymbolTable{
		Symbols: make(map[string]*ir.InstAlloca),
		Parent:  parent,
		Name:    name,
	}
}

// GetScopeName returns the name of the function
func (l *SymbolTable) GetScopeName() string {
	return l.Name
}

// GetEnclosingScope returns this scope's parent scope
func (l *SymbolTable) GetEnclosingScope() LLVMScope {
	return l.Parent
}

// Define insert a new symbol into the current scope
func (l *SymbolTable) Define(name string, inst *ir.InstAlloca) {
	l.Symbols[name] = inst
}

// Resolve retrieve the symbol associated with 'name' argument.
// If the symbol is not found in the current scope, Resolve will
// recursively search the parent scopes.
func (l *SymbolTable) Resolve(name string) *ir.InstAlloca {
	sym := l.Symbols[name]
	if sym != nil {
		return sym
	}

	if l.Parent != nil {
		return l.Parent.Resolve(name)
	}

	return nil
}
