package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// BoolLiteral defines a boolean literal value node in the AST
type BoolLiteral struct {
	TokenKind token.Kind
	Value     string
	EType     types.Type
}

func (b *BoolLiteral) String() string {
	return b.Value
}

// Accept ...
func (b *BoolLiteral) Accept(v Visitor) {
	v.VisitBoolLiteral(b)
}

// Type ...
func (b *BoolLiteral) Type() types.Type {
	return b.EType
}

func (b *BoolLiteral) expr() {}
