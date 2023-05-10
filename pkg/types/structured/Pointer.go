package structured

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Pointer models a pointer data type
type Pointer struct {
	Token      token.Token
	DomainType types.Type
}

// GetName ...
func (p *Pointer) GetName() string {
	return "pointer"
}
