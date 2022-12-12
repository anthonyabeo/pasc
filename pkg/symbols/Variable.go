package symbols

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/symbols/dtype"
)

// Variable denotes a variable symbol
type Variable struct {
	Name string
	Kind Kind
	Type dtype.Type
}

func (v *Variable) getKind() Kind {
	return v.Kind
}

func (v *Variable) getName() string {
	return v.Name
}

func (v *Variable) getType() dtype.Type {
	return v.Type
}

func (v *Variable) String() string {
	if v.Type != nil {
		return fmt.Sprintf("<%v:%v>", v.Name, v.Type)
	}

	return v.Name
}
