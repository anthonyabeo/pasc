package ast

import "fmt"

// CompoundStatement is the node in the AST that represents a compound statement.
type CompoundStatement struct {
	Statements []Statement
}

// TokenLiteral returns the text value this node's token.
func (cs *CompoundStatement) TokenLiteral() string { return "compound-statement" }

// StatNode ...
func (cs *CompoundStatement) StatNode() string {
	return fmt.Sprintf("%v", cs.Statements)
}

func (cs *CompoundStatement) String() string {
	return fmt.Sprintf("%v", cs.Statements)
}
