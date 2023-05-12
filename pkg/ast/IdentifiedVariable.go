package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

type IdentifiedVariable struct {
	PointerName *Identifier
	UnderType   types.Type
	EvalType    types.Type
}

// TokenLiteral returns the text value this node's token.
func (i *IdentifiedVariable) TokenLiteral() string {
	return "identified-variable"
}

func (i *IdentifiedVariable) exprNode() {}

// TokenKind returns this node's token's kind
func (i *IdentifiedVariable) TokenKind() token.Kind {
	return token.Caret
}

// Attr ...
func (i *IdentifiedVariable) Attr(attr string) any {
	switch attr {
	case "type":
		return i.EvalType
	default:
		return ""
	}
}

func (i *IdentifiedVariable) String() string {
	return fmt.Sprintf("%v", i.PointerName)
}
