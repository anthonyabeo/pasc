package structured

import (
	"go/types"

	"github.com/anthonyabeo/pasc/pkg/ast"
)

// SubRange ...
type SubRange struct {
	Start, End ast.Expression
	HostType   types.Type
}

// GetName ...
func (s *SubRange) GetName() string {
	return "sub-range"
}
