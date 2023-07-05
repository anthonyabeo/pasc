package structured

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Array ...
type Array struct {
	TokenKind     token.Kind
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
