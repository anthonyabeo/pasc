package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// IndexedVariable ...
type IndexedVariable struct {
	ArrayVar  Expression
	IndexExpr []Expression
	EvalType  types.Type
}

// TokenLiteral returns the text value this node's token.
func (id *IndexedVariable) TokenLiteral() string {
	return "indexed-variable"
}

func (id *IndexedVariable) exprNode() {}

// TokenKind returns this node's token's kind
func (id *IndexedVariable) TokenKind() token.Kind {
	return token.Array
}

// Attr ...
func (id *IndexedVariable) Attr(attr string) any {
	switch attr {
	case "type":
		return id.EvalType
	default:
		return ""
	}
}

func (id *IndexedVariable) String() string {
	var idxExprList []string
	for _, expr := range id.IndexExpr {
		idxExprList = append(idxExprList, expr.String())
	}
	return fmt.Sprintf(`array: %v[%v]`, id.ArrayVar.String(), strings.Join(idxExprList, ", "))
}
