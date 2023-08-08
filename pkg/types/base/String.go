package base

import "github.com/anthonyabeo/pasc/pkg/types"

type String struct {
	name          string
	NumComponents int
}

func NewString(numComp int) *String {
	return &String{name: "string", NumComponents: numComp}
}

// Name returns the character
func (s *String) Name() string {
	return s.name
}

func (s *String) String() string {
	return s.name
}

func (s *String) Ord() {}

func (s *String) Underlying() types.Type {
	return s
}
