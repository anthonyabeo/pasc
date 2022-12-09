package ast

import "fmt"

// Block is a block type node in the AST. These include function and procedure blocks
type Block struct {
	// label definition part
	// constant definition part
	// type definition part

	// variable declaration part
	VarDeclaration *VarDeclaration

	// procedure and function declaration part
	Callables []Statement

	// statements
	Stats []Statement
}

// TokenLiteral returns the text value this node's token.
func (b *Block) TokenLiteral() string { return "block" }

// StatNode ...
func (b *Block) StatNode() string {
	return fmt.Sprintf("%v\n%v\n%v\n", b.VarDeclaration, b.Callables, b.Stats)
}

func (b *Block) String() string {
	return fmt.Sprintf("%v\n%v\n%v\n", b.VarDeclaration, b.Callables, b.Stats)
}
