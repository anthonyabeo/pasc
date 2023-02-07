package types

// Integer is an integer data type
type Integer struct {
	Name string
}

// GetName ...
func (i *Integer) GetName() string {
	return i.Name
}

func (i *Integer) isBuiltIn() bool {
	return true
}
