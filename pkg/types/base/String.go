package base

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

func (s *String) isBuiltIn() bool {
	return true
}

func (s *String) Ord() {}
