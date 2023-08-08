package base

import "github.com/anthonyabeo/pasc/pkg/types"

// Integer is an integer data type
type Integer struct {
	name string
}

func NewInteger() *Integer {
	return &Integer{name: "integer"}
}

// Name ...
func (i *Integer) Name() string {
	return i.name
}

func (i *Integer) String() string {
	return i.name
}

func (i *Integer) Ord() {}

func (i *Integer) Underlying() types.Type {
	return i
}
