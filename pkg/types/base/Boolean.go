package base

// Boolean is an integer data type
type Boolean struct {
	Name string
}

// GetName ...
func (b *Boolean) GetName() string {
	return b.Name
}

func (b *Boolean) isBuiltIn() bool {
	return true
}

func (b *Boolean) Ord() {}
