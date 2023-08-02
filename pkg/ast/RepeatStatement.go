package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// RepeatStatement models the AST node of a Repeat Statement
type RepeatStatement struct {
	Token    token.Token
	StmtSeq  []Statement
	BoolExpr Expression
	Label    string
}

func (r *RepeatStatement) Accept(vst Visitor) error {
	return vst.VisitRepeatStatement(r)
}

func (r *RepeatStatement) Pos() *token.Position {
	return r.Token.Pos
}

// StatNode ...
func (r *RepeatStatement) stmt() {}

func (r *RepeatStatement) String() string {
	var stmtSeq []string
	for _, stmt := range r.StmtSeq {
		stmtSeq = append(stmtSeq, stmt.String())
	}

	return fmt.Sprintf("repeat\n\t%vuntil%v", strings.Join(stmtSeq, "\n\t"), r.BoolExpr)
}

func (r *RepeatStatement) SetLabel(l string) {
	r.Label = l
}
