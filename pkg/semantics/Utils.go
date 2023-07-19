package semantics

import (
	"strconv"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
	"github.com/anthonyabeo/pasc/pkg/types/structured"
)

// AreAssignmentCompatible checks whether the provided types are 'assignment-compatible'. Any types T1 is said to be
// 'assignment-compatible' with T2 if any of the following conditions are satisfied:
//
// a) T1 and T2 are the same type, and that type is permissible as the component-type of a file-type (see 6.4.3.5).
// b) T1 is the real-type and T2 is the integer-type.
// c) T1 and T2 are compatible ordinal-types, and the value of type T2 is in the closed interval specified by the type T1.
// d) T1 and T2 are compatible set-types, and all the members of the value of type T2 are in the closed interval specified by the base-type of T1.
// e) T1 and T2 are compatible string-types.
func (s *Visitor) AreAssignmentCompatible(src ast.Expression, dest ast.Expression) bool {
	if dest.Type().Name() == "real" && src.Type().Name() == "integer" {
		return true
	}

	if AreCompatibleTypes(src.Type(), dest.Type()) {
		if IsOrdinalType(src.Type()) && IsOrdinalType(dest.Type()) {
			switch dst := dest.Type().(type) {
			case *base.Integer:
				return src.Type().Name() == "integer"
			case *base.Char:
				return src.Type().Name() == "char"
			case *base.Boolean:
				return src.Type().Name() == "Boolean"
			case *structured.Enumerated:
				sym := s.symbolTable.RetrieveSymbol(src.String())
				if sym != nil && sym.Kind() == CONST {
					enum := sym.(*Const)
					if enum.typ.String() != dst.String() {
						return false
					}

					i, err := strconv.Atoi(enum.value.(*ast.UIntegerLiteral).Value)
					if err == nil && (0 <= i && i <= len(dst.List)-1) {
						return true
					}

					return false
				}
			case *structured.SubRange:
				var dstStartValue, dstEndValue int

				switch dst.HostType.(type) {
				case *structured.Enumerated:
					dstStartSym := s.symbolTable.RetrieveSymbol(dst.Range.Start.String())
					if dstStartSym == nil || dstStartSym.Kind() != CONST {
						return false
					}
					dstStartValue, _ = strconv.Atoi(dstStartSym.(*Const).value.(*ast.UIntegerLiteral).Value)

					dstEndSym := s.symbolTable.RetrieveSymbol(dst.Range.End.String())
					if dstEndSym == nil || dstEndSym.Kind() != CONST {
						return false
					}
					dstEndValue, _ = strconv.Atoi(dstEndSym.(*Const).value.(*ast.UIntegerLiteral).Value)
				case *base.Char:
					dstStartValue = int([]rune(dst.Range.Start.String())[0])
					dstEndValue = int([]rune(dst.Range.End.String())[0])
				default:
					dstStartValue, _ = strconv.Atoi(dst.Range.Start.String())
					dstEndValue, _ = strconv.Atoi(dst.Range.End.String())

				}

				if srcSubR, ok := src.Type().(*structured.SubRange); ok {
					var srcStartValue, srcEndValue int

					switch srcSubR.HostType.(type) {
					case *structured.Enumerated:
						srcStartSym := s.symbolTable.RetrieveSymbol(srcSubR.Range.Start.String())
						if srcStartSym == nil || srcStartSym.Kind() != CONST {
							return false
						}
						srcStartValue, _ = strconv.Atoi(srcStartSym.(*Const).value.(*ast.UIntegerLiteral).Value)

						srcEndSym := s.symbolTable.RetrieveSymbol(srcSubR.Range.End.String())
						if srcEndSym == nil || srcEndSym.Kind() != CONST {
							return false
						}
						srcEndValue, _ = strconv.Atoi(srcEndSym.(*Const).value.(*ast.UIntegerLiteral).Value)
					case *base.Char:
						srcStartValue = int([]rune(srcSubR.Range.Start.String())[0])
						srcEndValue = int([]rune(srcSubR.Range.End.String())[0])
					default:
						srcStartValue, _ = strconv.Atoi(srcSubR.Range.Start.String())
						srcEndValue, _ = strconv.Atoi(srcSubR.Range.End.String())
					}

					return (srcStartValue >= dstStartValue && srcStartValue <= dstEndValue) &&
						(srcEndValue >= dstStartValue && srcEndValue <= dstStartValue)

				} else {
					var value int

					if src.Type().Name() != dst.HostType.Name() {
						return false
					}

					switch src.Type().(type) {
					case *structured.Enumerated:
						sym := s.symbolTable.RetrieveSymbol(src.String())
						if sym == nil || sym.Kind() != CONST {
							return false
						}
						value, _ = strconv.Atoi(sym.(*Const).value.(*ast.UIntegerLiteral).Value)
					case *base.Char:
						value = int([]rune(src.String())[0])
					default:
						value, _ = strconv.Atoi(src.String())
					}

					if dstStartValue <= value && value <= dstStartValue {
						return true
					}
				}
			}

		}

		if src.Type().Name() == "set" && dest.Type().Name() == "set" {
			srcSet := src.Type().(*types.Set)
			dstSet := dest.Type().(*types.Set)

			return srcSet.BaseType.String() == dstSet.BaseType.String()
		}

		if src.Type().Name() == dest.Type().Name() {
			return true
		}
	}

	if src.Type().Name() == dest.Type().Name() /* && src.Type().Name() is permissible as the component-type of a file-type*/ {
		return true
	}

	return false
}

// AreCompatibleTypes checks whether type provided types are 'compatible'.
func AreCompatibleTypes(a, b types.Type) bool {
	if a.Name() == "string" && b.Name() == "string" {
		aStr := a.(*base.String)
		bStr := b.(*base.String)

		return aStr.NumComponents == bStr.NumComponents
	}

	if a.Name() == "set" && b.Name() == "set" {
		aSet := a.(*types.Set)
		bSet := b.(*types.Set)

		if aSet.BaseType == nil {
			aSet.BaseType = bSet.BaseType
			return true
		} else if bSet.BaseType == nil {
			bSet.BaseType = aSet.BaseType
			return true
		} else {
			return AreCompatibleTypes(aSet.BaseType, bSet.BaseType)
			//&& ((aSet.IsPacked && bSet.IsPacked) || (!aSet.IsPacked && !bSet.IsPacked))
		}
	}

	if a.Name() == "subrange" {
		aSubR := a.(*structured.SubRange)

		return aSubR.HostType.Name() == b.Name()
	} else if b.Name() == "subrange" {
		bSubR := b.(*structured.SubRange)

		return bSubR.HostType.Name() == a.Name()
	} else if a.Name() == "subrange" && b.Name() == "subrange" {
		aSubR := a.(*structured.SubRange)
		bSubR := b.(*structured.SubRange)

		return aSubR.HostType.Name() == bSubR.HostType.Name()
	}

	return a.Name() == b.Name()
}

func IsSimpleType(t types.Type) bool {
	return IsOrdinalType(t) || t.Name() == "real"
}

func IsOrdinalType(t types.Type) bool {
	return t.Name() == "enum" ||
		t.Name() == "integer" ||
		t.Name() == "char" ||
		t.Name() == "subrange" ||
		t.Name() == "Boolean"
}
