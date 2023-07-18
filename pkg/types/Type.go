package types

import "fmt"

// Type represents some data type
type Type interface {
	fmt.Stringer
	Name() string
}

// Ordinal is the generic parent of ordinal types
type Ordinal interface {
	Type
	Ord()
}
