package base

type String struct {
	name string
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
