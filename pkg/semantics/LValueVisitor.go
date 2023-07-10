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
func (l *LValueVisitor) VisitIdentifier(id *ast.Identifier) error {
	sym := l.symbolTable.RetrieveSymbol(id.Name)
	if sym == nil {
		return fmt.Errorf("undefined symbol %s", sym.Name())
	}

	if l.isLValue(sym) {
		//panic(fmt.Sprintf("cannot assign to '%s' (of type '%s'). It is not an l-value",
		//	sym.Name(), sym.Type()))
		return fmt.Errorf("cannot assign to '%s' (of type '%s'). It is not an l-value", sym.Name(), sym.Type())
	}

	id.EType = sym.Type()

	return nil
}

func (l *LValueVisitor) isLValue(sym Symbol) bool {
	return sym.Kind() != VARIABLE &&
		sym.Kind() != FIELD &&
		sym.Kind() != FUNCTION
}

// VisitIndexedVariable ...
func (l *LValueVisitor) VisitIndexedVariable(iv *ast.IndexedVariable) error {
	for _, idx := range iv.IndexExpr {
		if err := idx.Accept(l); err != nil {
			return err
		}

		if _, ok := idx.Type().(types.Ordinal); !ok {
			return fmt.Errorf("'%s' (of type '%s') cannot be used as index type in array %s", idx, idx.Type(), iv)
		}
	}

	array := l.symbolTable.RetrieveSymbol(iv.ArrayVar.String()).Type()
	for i := 0; i < len(iv.IndexExpr); i++ {
		arr := array.(*types.Array)
		if arr == nil {
			return fmt.Errorf("symbol %s is not declared", iv.ArrayVar.String())
		} else if arr.Name() != "array" {
			//panic(fmt.Sprintf("cannot index into non-array type, %s", iv.ArrayVar))
			return fmt.Errorf("cannot index into non-array type, %s", iv.ArrayVar)
		} else {
			array = arr.ComponentType
		}
	}

	iv.EType = array

	return nil
}

func (l *LValueVisitor) VisitFieldDesignator(f *ast.FieldDesignator) error {
	sym := l.symbolTable.RetrieveSymbol(f.RecordVar.String())
	if sym == nil {
		//panic(fmt.Sprintf("undefined name %s", f.RecordVar.String()))
		return fmt.Errorf("undefined name %s", f.RecordVar.String())
	} else if sym.Type().Name() != "record" {
		//panic(fmt.Sprintf("%s is not a record type", sym.Type().Name()))
		return fmt.Errorf("%s is not a record type", sym.Type().Name())
	} else {
		if !l.symbolTable.DeclaredLocally(f.FieldSpec.String()) {
			return fmt.Errorf("symbol %s not declared", f.FieldSpec.String())
		}

		if err := f.FieldSpec.Accept(l); err != nil {
			return err
		}

		f.EType = f.FieldSpec.Type()
	}

	return nil
}
