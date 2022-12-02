package ast

// CompoundStatement ...
type CompoundStatement struct {
	Statements []Statement
}

// TokenLiteral returns the text value this node's token.
func (cs *CompoundStatement) TokenLiteral() string { return "compound-statement" }

func (cs *CompoundStatement) statNode() {}
