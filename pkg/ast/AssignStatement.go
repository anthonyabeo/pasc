package ast

import "fmt"

// AssignStatement is the node used to represent Pascal assignments in the AST
type AssignStatement struct {
	Variable *Identifier
	Value    Expression
}

// TokenLiteral returns the text value this node's token.
func (as *AssignStatement) TokenLiteral() string {
	return as.Variable.Token.Text
}

func (as *AssignStatement) statNode() {}

func (as *AssignStatement) String() string {
	return fmt.Sprintf("%v := %v", as.Variable, as.Value)
}
