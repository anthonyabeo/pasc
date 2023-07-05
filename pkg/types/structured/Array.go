package structured

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"strings"
)

// Array ...
type Array struct {
	Token         token.Token
	Indices       []types.Ordinal
	ComponentType types.Type
}

// Name ...
func (a *Array) Name() string {
	return "array"
}

func (a *Array) String() string {
	var indices []string
	for _, idx := range a.Indices {
		indices = append(indices, idx.String())
	}

	return fmt.Sprintf("<array[%s] of %s>", strings.Join(indices, ", "), a.ComponentType)
}
