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

// Name ...
func (s *SubRange) Name() string {
	return "subrange"
}

func (s *SubRange) String() string {
	return s.Range.String()
}

func (s *SubRange) Ord() {}
