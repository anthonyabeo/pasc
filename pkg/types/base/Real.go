package base

// Real is an real data type
type Real struct {
	name string
}

// Name ...
func (r *Real) Name() string {
	return r.name
}

func NewReal(name string) *Real {
	return &Real{name: name}
}

func (r *Real) String() string {
	return r.name
}

func (r *Real) isBuiltIn() bool {
	return true
}
