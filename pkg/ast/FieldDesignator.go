package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"

	"github.com/anthonyabeo/pasc/pkg/types"
)

// FieldDesignator ...
type FieldDesignator struct {
	RecordVar Expression
	FieldSpec Expression
	EType     types.Type
}

func (f *FieldDesignator) Accept(vst Visitor) error {
	return vst.VisitFieldDesignator(f)
}

func (f *FieldDesignator) Pos() *token.Position {
	return f.RecordVar.Pos()
}

func (f *FieldDesignator) Type() types.Type {
	return f.EType
}

func (f *FieldDesignator) expr() {}

func (f *FieldDesignator) String() string {
	return fmt.Sprintf("%v.%v", f.RecordVar, f.FieldSpec)
}
