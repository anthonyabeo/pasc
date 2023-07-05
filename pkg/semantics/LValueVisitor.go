package semantics

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/ast"
)

// LValueVisitor ...
type LValueVisitor struct {
	Visitor
}

// VisitIdentifier ...
func (l *LValueVisitor) VisitIdentifier(id *ast.Identifier) {
	sym := l.symbolTable.RetrieveSymbol(id.Name)
	if sym == nil {
		panic(fmt.Sprintf("undefined symbol %s", sym.Name()))
	}

	if l.isLValue(sym) {
		panic(fmt.Sprintf("cannot assign to '%s' (of type '%s'). It is not an l-value",
			sym.Name(), sym.Type()))
	}

	id.EType = sym.Type()
}

func (l *LValueVisitor) isLValue(sym Symbol) bool {
	return sym.Kind() != VARIABLE &&
		sym.Kind() != FIELD &&
		sym.Kind() != FUNCTION
}

// VisitIndexedVariable ...
func (l *LValueVisitor) VisitIndexedVariable(iv *ast.IndexedVariable) {

}

func (l *LValueVisitor) VisitFieldDesignator(f *ast.FieldDesignator) {

}
