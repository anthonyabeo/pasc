package semantics

import (
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
		panic("undefined symbol")
	}

	if l.isLValue(sym) {
		panic("is not an l-value")
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
