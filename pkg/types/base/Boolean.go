package base

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

func (b *Boolean) Ord() {}
