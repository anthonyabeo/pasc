package ast

import "github.com/anthonyabeo/pasc/pkg/token"

type LabelDefinition struct {
	Token  token.Token
	Labels []*UIntegerLiteral
}
