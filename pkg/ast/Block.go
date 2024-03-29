package ast

import "fmt"

// Block is a block type node in the AST. These include function and procedure blocks
type Block struct {
	// label definition part
	Labels *LabelDefinition

	// constant definition part
	Consts *ConstDefinition

	// type definition part
	Types *TypeDefinition

	// variable declaration part
	VarDeclaration *VarDeclaration

	// procedure and function declaration part
	Callables []Statement

	// statements
	Stats []Statement
}

func (b *Block) String() string {
	return fmt.Sprintf("%v\n%v\n%v\n", b.VarDeclaration, b.Callables, b.Stats)
}
