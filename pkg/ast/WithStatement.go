package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// WithStatement models the AST node of a With Statement
type WithStatement struct {
	TokenKind     token.Kind
	RecordVarList []Expression
	Body          Statement
	Label         string
	EType         types.Type
}

func (w *WithStatement) Accept(v Visitor) {
	v.VisitWithStatement(w)
}

func (w *WithStatement) Type() types.Type {
	return w.EType
}

// StatNode ...
func (w *WithStatement) stmt() {}

func (w *WithStatement) String() string {
	var recVarList []string
	for _, recVar := range w.RecordVarList {
		recVarList = append(recVarList, recVar.String())
	}

	return fmt.Sprintf("with %v do %v", strings.Join(recVarList, ", "), w.Body)
}

func (w *WithStatement) SetLabel(l string) {
	w.Label = l
}
