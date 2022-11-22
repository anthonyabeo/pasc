package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// Type represents some data type
type Type interface {
	GetName() string
}

// TInteger is an integer data type
type TInteger struct {
	Token token.Token
}

// NewTInteger ...
func NewTInteger(tt token.Token) *TInteger {
	return &TInteger{Token: tt}
}

// GetName ...
func (i *TInteger) GetName() string {
	return i.Token.Text
}

// IsTypeIdentifier returns true if the token Type is a type as specified
// by the specification and false otherwise.
func IsTypeIdentifier(tt token.Type) bool {
	// TODO: Implement type ID checking
	return true
}
