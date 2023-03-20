package semantics

import (
	"github.com/anthonyabeo/pasc/pkg/ast"
)

// Visitor ...
type Visitor interface {
	Visit(ast.Node)
}

// Walk ...
func Walk(v Visitor, node ast.Node) {
	if node == nil {
		return
	}

	v.Visit(node)
}
