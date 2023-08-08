package base

import "github.com/anthonyabeo/pasc/pkg/types"

// Boolean is an integer data type
type Boolean struct {
	name string
}

func NewBoolean() *Boolean {
	return &Boolean{name: "Boolean"}
}

// Name ...
func (b *Boolean) Name() string {
	return b.name
}

func (b *Boolean) String() string {
	return b.name
}
func (b *Boolean) Underlying() types.Type {
	return b
}

func (b *Boolean) Ord() {}
