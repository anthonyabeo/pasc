package types

// Char models a data type for single characters
type Char struct {
	Name string
}

// GetName returns the character
func (c *Char) GetName() string {
	return c.Name
}

func (c *Char) isBuiltIn() bool {
	return true
}

func (c *Char) Ord() {}
