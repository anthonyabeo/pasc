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
	EvalType  types.Type
}

// TokenLiteral returns the text value this node's token.
func (f *FieldDesignator) TokenLiteral() string {
	return "indexed-variable"
}

func (f *FieldDesignator) exprNode() {}

// TokenKind returns this node's token's kind
func (f *FieldDesignator) TokenKind() token.Kind {
	return token.Record
}

// Attr ...
func (f *FieldDesignator) Attr(attr string) any {
	switch attr {
	case "type":
		return f.EvalType
	default:
		return ""
	}
}

func (f *FieldDesignator) String() string {
	return fmt.Sprintf("%v.%v", f.RecordVar, f.FieldSpec)
}
