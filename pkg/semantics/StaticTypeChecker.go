package semantics

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// StaticTypeCheckVisitor ...
type StaticTypeCheckVisitor struct {
}

// Visit ...
func (v *StaticTypeCheckVisitor) Visit(node ast.Node) {
	var err error

	switch node := node.(type) {
	case *ast.FuncDeclaration:
	case *ast.ProcedureStatement:
	case *ast.AssignStatement:
		if !areAssignmentCompatible(node.Variable.Attr("type").(types.Type), node.Value.Attr("type").(types.Type)) {
			err = fmt.Errorf(
				"TypeError: cannot assign value of type %s to variable of type %s",
				node.Value.Attr("type"), node.Variable.Attr("type"),
			)
			panic(err)
		}
	case *ast.IfStatement:
		if node.BoolExpr.Attr("type").(types.Type).GetName() != "Boolean" {
			err = fmt.Errorf("if-statement condition does not evaluate to boolean type")
			panic(err)
		}
	default:
		panic(fmt.Sprintf("Visit: unexpected node type %T", node))
	}
}

// TODO complete implementation
// a) T1 and T2 are the same type.
// b) T1 is a subrange of T2, or T2 is a subrange of T1, or both T1 and T2 are subranges of the same host-type.
// c) T1 and T2 are set-types of compatible base-types, and either both T1 and T2 are designated packed or neither T1 nor T2 is designated packed.
// d) T1 and T2 are string-types with the same number of components.
func areCompatibleTypes(a, b types.Type) bool {
	if equals(a, b) {
		return true
	}

	return false
}

// TODO complete implementation
// a) T1 and T2 are the same type, and that type is permissible as the component-type of a file-type (see 6.4.3.5).
// b) T1 is the real-type and T2 is the integer-type.
// c) T1 and T2 are compatible ordinal-types, and the value of type T2 is in the closed interval specified by the type T1.
// d) T1 and T2 are compatible set-types, and all the members of the value of type T2 are in the closed interval speci ed by the base-type of T1.
// e) T1 and T2 are compatible string-types.
func areAssignmentCompatible(src, dest types.Type) bool {
	if dest.GetName() == "real" && src.GetName() == "integer" {
		return true
	}

	if areCompatibleTypes(src, dest) {
		return true
	}

	return false
}

func equals(a, b types.Type) bool {
	return a.GetName() == b.GetName()
}

func isSimpleType(t types.Type) bool {
	return t.GetName() == "integer" ||
		t.GetName() == "char" ||
		t.GetName() == "Boolean" ||
		t.GetName() == "enum" ||
		t.GetName() == "subrange"
}
