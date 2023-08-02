package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/types"
)

// IndexedVariable ...
type IndexedVariable struct {
	ArrayVar  Expression
	IndexExpr []Expression
	EType     types.Type
}

func (iv *IndexedVariable) Accept(vst Visitor) error {
	return vst.VisitIndexedVariable(iv)
}

func (iv *IndexedVariable) Pos() *token.Position {
	return iv.ArrayVar.Pos()
}

func (iv *IndexedVariable) Type() types.Type {
	return iv.EType
}

func (iv *IndexedVariable) expr() {}

func (iv *IndexedVariable) String() string {
	var idxExprList []string
	for _, expr := range iv.IndexExpr {
		idxExprList = append(idxExprList, expr.String())
	}
	return fmt.Sprintf(`%v[%v]`, iv.ArrayVar.String(), strings.Join(idxExprList, ", "))
}
