package structured

import (
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Record denoted a record type
type Record struct {
	TokenKind token.Kind
	FieldList []Field
}

// Name ...
func (r *Record) Name() string {
	return "record"
}

func (r *Record) String() string {
	return "Record-Type-to-be-implemented"
}

func (r *Record) Underlying() types.Type {
	return r
}

// RecordSection ...
type RecordSection struct {
	List []*ast.Identifier
	Type types.Type
}

// Field ...
type Field interface {
	recordField()
}

// FixedPart ...
type FixedPart struct {
	Entry []*RecordSection
}

func (f *FixedPart) recordField() {}

// VariantPart ...
type VariantPart struct {
	Token           token.Token
	VariantSelector *VariantSelector
	Variants        []*Variant
}

func (v *VariantPart) recordField() {}

// Variant ...
type Variant struct {
	CaseConstList []ast.Expression
	FieldList     []Field
}

// VariantSelector ...
type VariantSelector struct {
	TagField *ast.Identifier
	TagType  types.Type
}
