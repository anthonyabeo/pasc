package symbols

// Function denotes a variable symbol
type Function struct {
	Name  string
	Kind  Kind
	Scope Scope
}

// NewFunctionSymbol ...
func NewFunctionSymbol(name string, kind Kind, scope Scope) *Function {
	return &Function{
		Name:  name,
		Kind:  kind,
		Scope: scope,
	}
}

func (f *Function) getKind() Kind {
	return f.Kind
}

func (f *Function) getName() string {
	return f.Name
}

func (f *Function) String() string {
	return f.Name
}
