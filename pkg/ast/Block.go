package ast

import "fmt"

// Block is a block type node in the AST. These include function and procedure blocks
type Block struct {
	// label definition part
	// constant definition part
	// type definition part

	// variable declaration part
	Vars []*VarDecl

	// procedure and function declaration part
	Callables []Statement

	// statements
	Stats []Statement
}

// TokenLiteral returns the text value this node's token.
func (b *Block) TokenLiteral() string { return "block" }

func (b *Block) statNode() {}

func (b *Block) String() string {
	return fmt.Sprintf("%v\n%v\n%v\n", b.Vars, b.Callables, b.Stats)
}
