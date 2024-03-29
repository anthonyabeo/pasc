package ast

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Node defines a generic node in the AST
type Node interface {
	fmt.Stringer
	Accept(Visitor) error
	Pos() *token.Position
}

// Statement models a generic node for statement types
type Statement interface {
	Node
	stmt()
	SetLabel(string)
}

// Expression models a generic node for expression types
type Expression interface {
	Node
	Type() types.Type
	expr()
}

type Decl interface {
	Node
	decl()
}
