package base

// Boolean is an integer data type
type Boolean struct {
	name string
}

func NewBoolean(name string) *Boolean {
	return &Boolean{name: name}
}

// Name ...
func (b *Boolean) Name() string {
	return b.name
}

func (b *Boolean) String() string {
	return b.name
}

func (b *Boolean) isBuiltIn() bool {
	return true
}

func (b *Boolean) Ord() {}
