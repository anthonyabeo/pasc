package types

// Real is an real data type
type Real struct {
	Name string
}

// GetName ...
func (r *Real) GetName() string {
	return r.Name
}

func (r *Real) isBuiltIn() bool {
	return true
}
