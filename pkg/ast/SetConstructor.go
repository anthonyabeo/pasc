package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// SetConstructor ...
type SetConstructor struct {
	Token   token.Token
	Members []Expression
	EType   types.Type
}

func (s *SetConstructor) expr() {}

func (s *SetConstructor) Accept(vst Visitor) error {
	return vst.VisitSetConstructor(s)
}

func (s *SetConstructor) Pos() *token.Position {
	return s.Token.Pos
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
