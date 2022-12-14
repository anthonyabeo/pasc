package symbols

// BuiltInType denotes a variable symbol
type BuiltInType struct {
	Name string
	Kind Kind
}

// GetKind returns the kind of this symbol
func (b *BuiltInType) GetKind() Kind {
	return b.Kind
}

// GetName returns the name of this symbol
func (b *BuiltInType) GetName() string {
	return b.Name
}

func (b *BuiltInType) String() string {
	return b.Name
}
