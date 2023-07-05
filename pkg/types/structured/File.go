package structured

import (
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// File models a file data type
type File struct {
	TokenKind     token.Kind
	ComponentType types.Type
}

// Name ...
func (f *File) Name() string {
	return "file"
}

func (f *File) String() string {
	return "file"
}
