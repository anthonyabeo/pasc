package structured

import (
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// SubRange ...
type SubRange struct {
	Range    *ast.Range
	HostType types.Type
}

// GetName ...
func (s *SubRange) GetName() string {
	return "subrange"
}

func (s *SubRange) Ord() {}
