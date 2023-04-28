package symbols

import "github.com/anthonyabeo/pasc/pkg/types"

// Label denotes a label symbol
type Label struct {
	Name string
	Kind Kind
	Type types.Type
}

// NewLabel returns a new label symbol
func NewLabel(name string, kind Kind, typ types.Type) *Label {
	return &Label{Name: name, Kind: kind, Type: typ}
}

// GetKind returns the kind of this symbol
func (l *Label) GetKind() Kind {
	return l.Kind
}

// GetName returns the name of this symbol
func (l *Label) GetName() string {
	return l.Name
}

// GetType returns the type of this symbol
func (l *Label) GetType() types.Type {
	return l.Type
}

func (l *Label) String() string {
	return l.Name
}
