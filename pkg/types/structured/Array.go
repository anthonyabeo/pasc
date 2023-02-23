package structured

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Array ...
type Array struct {
	Token         token.Token
	Indices       []types.Ordinal
	ComponentType types.Type
}

// GetName ...
func (a *Array) GetName() string {
	return "array"
}
