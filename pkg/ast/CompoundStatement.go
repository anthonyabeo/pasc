package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// CompoundStatement is the node in the AST that represents a compound statement.
type CompoundStatement struct {
	Statements []Statement
}

// TokenLiteral returns the text value this node's token.
func (cs *CompoundStatement) TokenLiteral() string { return "compound-statement" }

// TokenKind returns this node's token's kind
func (cs *CompoundStatement) TokenKind() token.Kind { return token.Procedure }

// StatNode ...
func (cs *CompoundStatement) StatNode() string {
	return fmt.Sprintf("%v", cs.Statements)
}

func (cs *CompoundStatement) String() string {
	return fmt.Sprintf("%v", cs.Statements)
}
