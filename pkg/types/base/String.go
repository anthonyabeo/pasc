package base

type String struct {
	Name string
}

// GetName returns the character
func (s *String) GetName() string {
	return s.Name
}

func (s *String) isBuiltIn() bool {
	return true
}

func (s *String) Ord() {}
