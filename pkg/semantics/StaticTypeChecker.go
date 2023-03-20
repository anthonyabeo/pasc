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
