package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/types"
)

type IdentifiedVariable struct {
	PointerName *Identifier
	UnderType   types.Type
	EType       types.Type
}

func (i *IdentifiedVariable) Accept(v Visitor) {
	v.VisitIdentifiedVariable(i)
}

func (i *IdentifiedVariable) Type() types.Type {
	return i.EType
}

func (i *IdentifiedVariable) expr() {}

func (i *IdentifiedVariable) String() string {
	return fmt.Sprintf("%v", i.PointerName)
}
