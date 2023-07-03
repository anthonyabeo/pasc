package ast

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// URealLiteral represents an unsigned floating point number
type URealLiteral struct {
	TokenKind token.Kind
	Value     string
	EType     types.Type
}

func (ur *URealLiteral) expr() {}

func (ur *URealLiteral) String() string {
	return ur.Value
}

func (ur *URealLiteral) Accept(v Visitor) {
	v.VisitURealLiteral(ur)
}

func (ur *URealLiteral) Type() types.Type {
	return ur.EType
}
