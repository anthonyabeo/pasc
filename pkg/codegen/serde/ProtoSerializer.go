package serde

import (
	"fmt"
	"io/ioutil"
	"os"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"google.golang.org/protobuf/proto"
)

// AstToProtoAst transforms the Go AST into a form that can be
// serialized into protocol buffers
func AstToProtoAst(Ast ast.ProgramAST) *Program {
	// TODO Remove hardcoding and actually implement function
	program := &Program{
		Kind:   TokenKind_PROGRAM,
		Name:   "HelloWorld",
		Params: []string{"input", "output"},
		Block: &Block{
			Stmts: []*Statement{
				{
					Kind: TokenKind_ASSIGN,
					Stmt: &Statement_AssignStmt{
						AssignStmt: &AssignStmt{
							Variable: "a",
							Value:    1,
						},
					},
				},
			},
		},
	}

	return program
}

// Serialize accepts `program`, an AST created from calling `serde.AstToProtoAst`,
// and converts it into a protol buffers binary file, to be deserialized later.
func Serialize(program *Program) error {
	out, err := proto.Marshal(program)
	if err != nil {
		return fmt.Errorf("Serialization error: %s", err.Error())
	}

	if err = os.Mkdir("out", 0755); err != nil {
		return err
	}

	fileName := fmt.Sprintf("out/%s.bin", program.Name)
	if err := ioutil.WriteFile(fileName, out, 0644); err != nil {
		return fmt.Errorf("Write File Error: %s ", err.Error())
	}

	return nil
}
