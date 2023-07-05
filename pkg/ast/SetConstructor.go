package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"strings"
)

// SetConstructor ...
type SetConstructor struct {
	TokenKind token.Kind
	Members   []Expression
	EType     types.Type
}

func (s *SetConstructor) expr() {}

func (s *SetConstructor) Accept(v Visitor) {
	v.VisitSetConstructor(s)
}

func (s *SetConstructor) Type() types.Type {
	return s.EType
}

func (s *SetConstructor) String() string {
	var members []string
	for _, m := range s.Members {
		members = append(members, m.String())
	}
	
	return fmt.Sprintf("[%s]", strings.Join(members, ", "))
}
