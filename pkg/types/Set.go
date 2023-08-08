package types

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// Set models a set data type
type Set struct {
	TokenKind token.Kind
	BaseType  Ordinal
}

// Name ...
func (s *Set) Name() string {
	return "set"
}

func (s *Set) String() string {
	return fmt.Sprintf("set of %s", s.BaseType)
}

func (s *Set) Underlying() Type {
	return s
}
