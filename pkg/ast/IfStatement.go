package ast

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// IfStatement is the node in the AST that represents an If Statement
type IfStatement struct {
	Token    token.Token
	BoolExpr Expression
	TruePath Statement
	ElsePath Statement
}

// TokenLiteral returns the text value this node's token.
func (f *IfStatement) TokenLiteral() string { return f.Token.Text }

// StatNode ...
func (f *IfStatement) StatNode() string {
	return fmt.Sprintf("if(%v) then %v else %v", f.BoolExpr, f.TruePath, f.ElsePath)
}

func (f *IfStatement) String() string {
	return fmt.Sprintf("if(%v) then %v else %v", f.BoolExpr, f.TruePath, f.ElsePath)
}
