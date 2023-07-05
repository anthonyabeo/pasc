package base

// Integer is an integer data type
type Integer struct {
	name string
}

func NewInteger(name string) *Integer {
	return &Integer{name: name}
}

// Name ...
func (i *Integer) Name() string {
	return i.name
}

func (i *Integer) String() string {
	return i.name
}

func (i *Integer) isBuiltIn() bool {
	return true
}

func (i *Integer) Ord() {}
