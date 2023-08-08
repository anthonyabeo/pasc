package base

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
