package types

import "github.com/anthonyabeo/pasc/pkg/token"

// Type represents some data type
type Type interface {
	GetName() string
}

// IsTypeIdentifier returns true if the token Type is a type as specified
// by the specification and false otherwise.
func IsTypeIdentifier(tt token.Kind) bool {
	// TODO: Implement type ID checking
	return true
}

type BuiltInType struct {
	Name string
}

func (b *BuiltInType) GetName() string {
	return b.Name
}
