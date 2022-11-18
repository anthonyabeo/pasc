package ast

// Node defines a generic node in the AST
type Node interface {
	TokenLiteral() string
}

// Statement models a generic node for statement types
type Statement interface {
	Node
	statNode()
}

// Expression models a generic node for expression types
type Expression interface {
	Node
	exprNode()
}
