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

func (v *Variable) getKind() Kind {
	return v.Kind
}

func (v *Variable) getName() string {
	return v.Name
}

func (v *Variable) String() string {
	return v.Name
}
