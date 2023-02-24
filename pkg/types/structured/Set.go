package structured

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Set models a set data type
type Set struct {
	Token    token.Token
	BaseType types.Ordinal
}

// GetName ...
func (s *Set) GetName() string {
	return "set"
}
