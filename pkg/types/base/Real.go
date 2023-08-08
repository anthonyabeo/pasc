package base

import "github.com/anthonyabeo/pasc/pkg/types"

// Real is an real data type
type Real struct {
	name string
}

// Name ...
func (r *Real) Name() string {
	return r.name
}

func NewReal() *Real {
	return &Real{name: "real"}
}

func (r *Real) String() string {
	return r.name
}

func (r *Real) Underlying() types.Type {
	return r
}
