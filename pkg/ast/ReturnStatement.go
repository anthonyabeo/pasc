package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

type ReturnStatement struct {
	TokenKind token.Kind
	Expr      Expression
	Label     string
}

func (r *ReturnStatement) Accept(vst Visitor) error {
	return vst.VisitReturnStatement(r)
}

func (r *ReturnStatement) stmt() {}

func (r *ReturnStatement) String() string {
	return fmt.Sprintf("return %s", r.Expr)
}

func (r *ReturnStatement) SetLabel(l string) {
	r.Label = l
}
