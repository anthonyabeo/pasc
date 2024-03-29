package base

import "github.com/anthonyabeo/pasc/pkg/types"

// Char models a data type for single characters
type Char struct {
	name string
}

func NewChar() *Char {
	return &Char{name: "char"}
}

// Name returns the character
func (c *Char) Name() string {
	return c.name
}

func (c *Char) String() string {
	return c.name
}

func (c *Char) Ord() {}

func (c *Char) Underlying() types.Type {
	return c
}
