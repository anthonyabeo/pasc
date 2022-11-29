package ast

// AssignStatement ...
type AssignStatement struct {
	Variable *Identifier
	Value    Expression
}

// NewAssignmentStatement ...
func NewAssignmentStatement(varID *Identifier) *AssignStatement {
	return &AssignStatement{Variable: varID}
}

// TokenLiteral returns the text value this node's token.
func (as *AssignStatement) TokenLiteral() string {
	return as.Variable.Token.Text
}

func (as *AssignStatement) statNode() {}
