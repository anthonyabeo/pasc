package structured

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Set models a set data type
type Set struct {
	Token    token.Token
	BaseType types.Ordinal
}

// Name ...
func (s *Set) Name() string {
	return "set"
}

func (s *Set) String() string {
	return fmt.Sprintf("set of %s", s.BaseType)
}
