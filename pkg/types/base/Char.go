package base

// Char models a data type for single characters
type Char struct {
	name string
}

func NewChar(name string) *Char {
	return &Char{name: name}
}

// Name returns the character
func (c *Char) Name() string {
	return c.name
}

func (c *Char) String() string {
	return c.name
}

func (c *Char) isBuiltIn() bool {
	return true
}

func (c *Char) Ord() {}
