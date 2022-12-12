package symbols

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/symbols/dtype"
)

// BuiltInType denotes a variable symbol
type BuiltInType struct {
	Name string
	Kind Kind
	Type dtype.Type
}

func (b *BuiltInType) getKind() Kind {
	return b.Kind
}

func (b *BuiltInType) getName() string {
	return b.Name
}

func (b *BuiltInType) getType() dtype.Type {
	return b.Type
}

// GetName ...
func (b *BuiltInType) GetName() string {
	return b.Name
}

func (b *BuiltInType) String() string {
	if b.Type != nil {
		return fmt.Sprintf("<%v:%v>", b.Name, b.Type)
	}

	return b.Name
}
