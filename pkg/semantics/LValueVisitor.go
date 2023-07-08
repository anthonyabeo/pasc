package semantics

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
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
	for _, idx := range iv.IndexExpr {
		idx.Accept(l)

		if _, ok := idx.Type().(types.Ordinal); !ok {
			panic(fmt.Sprintf("'%s' (of type '%s') cannot be used as index type in array %s",
				idx, idx.Type(), iv))
		}
	}

	array := l.symbolTable.RetrieveSymbol(iv.ArrayVar.String()).Type()
	for i := 0; i < len(iv.IndexExpr); i++ {
		arr := array.(*types.Array)
		if arr == nil {
			panic(fmt.Sprintf(""))
		} else if arr.Name() != "array" {
			panic(fmt.Sprintf("cannot index into non-array type, %s", iv.ArrayVar))
		} else {
			array = arr.ComponentType
		}
	}

	iv.EType = array
}

func (l *LValueVisitor) VisitFieldDesignator(f *ast.FieldDesignator) {
	sym := l.symbolTable.RetrieveSymbol(f.RecordVar.String())
	if sym == nil {
		panic(fmt.Sprintf("undefined name %s", f.RecordVar.String()))
	} else if sym.Type().Name() != "record" {
		panic(fmt.Sprintf("%s is not a record type", sym.Type().Name()))
	} else {
		if !l.symbolTable.DeclaredLocally(f.FieldSpec.String()) {
			panic(fmt.Sprintf(""))
		}

		f.FieldSpec.Accept(l)
		f.EType = f.FieldSpec.Type()
	}
}
