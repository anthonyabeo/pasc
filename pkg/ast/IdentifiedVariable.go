package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

type IdentifiedVariable struct {
	PointerName *Identifier
	UnderType   types.Type
	EType       types.Type
}

func (i *IdentifiedVariable) Accept(vst Visitor) error {
	return vst.VisitIdentifiedVariable(i)
}

func (i *IdentifiedVariable) Pos() *token.Position {
	return i.PointerName.Pos()
}

func (i *IdentifiedVariable) Type() types.Type {
	return i.EType
}

func (i *IdentifiedVariable) expr() {}

func (i *IdentifiedVariable) String() string {
	return fmt.Sprintf("%v", i.PointerName)
}
