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

// BuiltIn represents a generic non-user-defined data type
type BuiltIn interface {
	Type
	isBuiltIn() bool
}

// UserDefined represents a generic user-defined type
type UserDefined interface {
	Type
	isUserDefined() bool
}
