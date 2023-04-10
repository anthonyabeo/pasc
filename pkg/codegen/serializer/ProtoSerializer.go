package serializer

import (
	"fmt"
	"io/ioutil"
	"os"
	"strconv"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
	"google.golang.org/protobuf/proto"
)

// AstToProtoAst transforms the Go AST into a form that can be
// serialized into protocol buffers
func AstToProtoAst(Ast ast.ProgramAST) *Program {
	program := &Program{Kind: TokenKind_PROGRAM, Name: Ast.Name.Name}
	for _, param := range Ast.ParamList {
		program.Params = append(program.Params, param.Name)
	}

	program.Block = translateBlock(Ast.Block)

	return program
}

func translateBlock(blk *ast.Block) *Block {
	block := &Block{}

	if blk.Labels != nil {

	}

	if blk.Consts != nil {

	}

	if blk.Types != nil {

	}

	if blk.VarDeclaration != nil {
		for _, declr := range blk.VarDeclaration.Decls {
			for _, name := range declr.Names {
				v := &VarDeclaration{
					Name: translateExpr(name),
					Type: translateType(declr.Type),
				}
				block.VarDeclrs = append(block.VarDeclrs, v)
			}
		}
	}

	if blk.Callables != nil {

	}

	if blk.Stats != nil {
		for _, stmt := range blk.Stats {
			block.Stmts = append(block.Stmts, translateStmt(stmt))
		}
	}

	return block
}

func translateStmt(stmt ast.Statement) *Statement {
	var s *Statement

	switch stmt := stmt.(type) {
	case *ast.AssignStatement:
		s = &Statement{
			Kind: TokenKind_ASSIGN,
			Stmt: &Statement_AssignStmt{
				AssignStmt: &AssignStmt{
					Variable: translateExpr(stmt.Variable),
					Value:    translateExpr(stmt.Value),
				},
			},
		}

	case *ast.ProcedureStatement:
		var args []*Expression
		for _, e := range stmt.ParamList {
			args = append(args, translateExpr(e))
		}

		s = &Statement{
			Kind: TokenKind_PROCEDURE,
			Stmt: &Statement_ProcStmt{
				ProcStmt: &ProcedureStmt{
					Name: translateExpr(stmt.ProcedureID),
					Args: args,
				},
			},
		}
	default:
		panic(fmt.Sprintf("Unimplemented %v", stmt))
	}

	return s
}

func translateExpr(expr ast.Expression) *Expression {
	var e *Expression

	switch expr := expr.(type) {
	case *ast.Identifier:
		e = &Expression{
			Kind: TokenKind_IDENTIFIER,
			Expr: &Expression_Id{Id: &Identifier{Name: expr.Name}},
		}
	case *ast.UIntegerLiteral:
		v, err := strconv.Atoi(expr.Value)
		if err != nil {
			panic(err)
		}

		e = &Expression{
			Kind: TokenKind_UINTLIT,
			Expr: &Expression_Uint{
				Uint: &UIntLiteral{
					Value: int32(v),
				},
			},
		}
	default:
		panic(fmt.Sprintf("Unimplemented %v", expr))
	}

	return e
}

func translateType(typ types.Type) *Type {
	var t *Type

	switch typ := typ.(type) {
	case *base.Integer:
		t = &Type{
			Tk:   TypeKind_INTEGER,
			Type: &Type_Int{Int: &Integer{Name: typ.GetName()}},
		}
	default:
		panic(fmt.Sprintf("Unimplemented %v", typ))
	}

	return t
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
