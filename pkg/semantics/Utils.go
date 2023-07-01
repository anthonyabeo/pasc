package semantics

import (
	"github.com/anthonyabeo/pasc/pkg/types"
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
	if src.GetName() == dest.GetName() {
		return true
	}

	return false
}

// AreCompatibleTypes checks whether type provided types are 'compatible'. Two types T1 and T2 are 'compatible' if
// any of the follow conditions are satisfied:
//
// a) T1 and T2 are the same type.
// b) T1 is a subrange of T2, or T2 is a subrange of T1, or both T1 and T2 are subranges of the same host-type.
// c) T1 and T2 are set-types of compatible base-types, and either both T1 and T2 are designated packed or neither T1 nor T2 is designated packed.
// d) T1 and T2 are string-types with the same number of components.
// TODO complete implementation
func AreCompatibleTypes(a, b types.Type) bool {
	if a.GetName() == b.GetName() {
		return true
	}

	return false
}
