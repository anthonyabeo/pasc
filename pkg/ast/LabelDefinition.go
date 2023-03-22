package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// LabelDefinition ...
type LabelDefinition struct {
	Token  token.Token
	Labels []*UIntegerLiteral
}
