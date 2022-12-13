package symbols

// BuiltInType denotes a variable symbol
type BuiltInType struct {
	Name string
	Kind Kind
}

func (b *BuiltInType) getKind() Kind {
	return b.Kind
}

func (b *BuiltInType) getName() string {
	return b.Name
}

// GetName ...
func (b *BuiltInType) GetName() string {
	return b.Name
}

func (b *BuiltInType) String() string {
	return b.Name
}
