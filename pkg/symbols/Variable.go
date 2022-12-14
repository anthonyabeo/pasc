package symbols

// Variable denotes a variable symbol
type Variable struct {
	Name string
	Kind Kind
}

// NewVariableSymbol ...
func NewVariableSymbol(name string, kind Kind) *Variable {
	return &Variable{Name: name, Kind: kind}
}

// GetKind returns the kind of this symbol
func (v *Variable) GetKind() Kind {
	return v.Kind
}

// GetName returns the name of this symbol
func (v *Variable) GetName() string {
	return v.Name
}

func (v *Variable) String() string {
	return v.Name
}
