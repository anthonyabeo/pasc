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
}

// TokenLiteral returns the text value this node's token.
func (r *RepeatStatement) TokenLiteral() string { return r.Token.Text }

// TokenKind returns this node's token's kind
func (r *RepeatStatement) TokenKind() token.Kind { return r.Token.Kind }

// StatNode ...
func (r *RepeatStatement) StatNode() string {
	var stmtSeq []string
	for _, stmt := range r.StmtSeq {
		stmtSeq = append(stmtSeq, stmt.StatNode())
	}

	return fmt.Sprintf("repeat\n\t%vuntil%v", strings.Join(stmtSeq, "\n\t"), r.BoolExpr)
}

func (r *RepeatStatement) String() string {
	var stmtSeq []string
	for _, stmt := range r.StmtSeq {
		stmtSeq = append(stmtSeq, stmt.StatNode())
	}

	return fmt.Sprintf("repeat\n\t%vuntil%v", strings.Join(stmtSeq, "\n\t"), r.BoolExpr)
}
