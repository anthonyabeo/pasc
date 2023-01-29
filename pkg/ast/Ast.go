package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// Node defines a generic node in the AST
type Node interface {
	TokenLiteral() string
	TokenKind() token.Kind
}

// Statement models a generic node for statement types
type Statement interface {
	Node
	StatNode() string
}

// Expression models a generic node for expression types
type Expression interface {
	Node
	exprNode()
	Attr(string) interface{}
	RValue() Expression
	LValue() Expression
	String() string
}
