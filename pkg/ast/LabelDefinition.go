package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// LabelDefinition ...
type LabelDefinition struct {
	Token  token.Token
	Labels []*UIntegerLiteral
}

func (l *LabelDefinition) Accept(vst Visitor) error {
	return vst.VisitLabelDef(l)
}

func (l *LabelDefinition) decl() {}

func (l *LabelDefinition) String() string {
	var labels []string
	for _, label := range l.Labels {
		labels = append(labels, label.Value)
	}

	return fmt.Sprintf("label %v;", strings.Join(labels, ", "))
}

func (l *LabelDefinition) Pos() *token.Position {
	return l.Token.Pos
}
