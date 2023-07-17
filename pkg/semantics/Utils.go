package semantics

import (
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
// TODO complete implementation
func AreAssignmentCompatible(src types.Type, dest types.Type) bool {
	if src.Name() == dest.Name() {
		return true
	}

	if dest.Name() == "real" && src.Name() == "integer" {
		return true
	}

	return false
}

// AreCompatibleTypes checks whether type provided types are 'compatible'.
func AreCompatibleTypes(a, b types.Type) bool {
	if a.Name() == "string" && b.Name() == "string" {
		aStr := a.(*base.String)
		bStr := b.(*base.String)

		return len(aStr.String()) == len(bStr.String())
	}

	if a.Name() == "set" && b.Name() == "set" {
		aSet := a.(*types.Set)
		bSet := b.(*types.Set)

		return AreCompatibleTypes(aSet.BaseType, bSet.BaseType)
		//&& ((aSet.IsPacked && bSet.IsPacked) || (!aSet.IsPacked && !bSet.IsPacked))
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
