package parser

import (
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
)

func (p *Parser) getTypeOf(e ast.Expression) types.Type {
	switch e := e.(type) {
	case *ast.UIntegerLiteral:
		return &base.Integer{Name: "integer"}
	case *ast.URealLiteral:
		return &base.Real{Name: "real"}
	case *ast.Identifier:
		return e.Scope.Resolve(e.Name).GetType()
	}

	return nil
}
