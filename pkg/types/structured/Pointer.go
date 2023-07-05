package structured

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Pointer models a pointer data type
type Pointer struct {
	TokenKind  token.Kind
	DomainType types.Type
}

// Name ...
func (p *Pointer) Name() string {
	return "pointer"
}

func (p *Pointer) String() string {
	return "pointer"
}
