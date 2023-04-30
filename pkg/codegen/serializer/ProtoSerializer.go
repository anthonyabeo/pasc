package serializer

import (
	"errors"
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
	"google.golang.org/protobuf/proto"
	"os"
	"strconv"
)

// AstToProtoAst transforms the Go AST into a form that can be
// serialized into protocol buffers
func AstToProtoAst(Ast ast.Program) *Program {
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
		for _, decl := range blk.VarDeclaration.Decls {
			for _, name := range decl.Names {
				v := &VarDeclaration{
					Name: translateExpr(name),
					Type: translateType(decl.Type),
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
	case *ast.IfStatement:
		ifStatement := &IfStmt{
			Cond:     translateExpr(stmt.BoolExpr),
			TruePath: translateStmt(stmt.TruePath),
		}

		if stmt.ElsePath != nil {
			ifStatement.ElsePath = translateStmt(stmt.ElsePath)
		}

		s = &Statement{
			Kind: TokenKind_IF,
			Stmt: &Statement_IfStmt{
				IfStmt: ifStatement,
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
			Kind: Expression_Ident,
			Expr: &Expression_Id{
				Id: &Identifier{
					Kind: Identifier_EntireVar,
					Value: &Identifier_Var{
						Var: &Identifier_Variable{
							Name: expr.Name},
					},
				},
			},
		}
	case *ast.UIntegerLiteral:
		v, err := strconv.Atoi(expr.Value)
		if err != nil {
			panic(err)
		}

		e = &Expression{
			Kind: Expression_UInt,
			Expr: &Expression_Uint{
				Uint: &UIntLiteral{
					Value: int32(v),
				},
			},
		}
	case *ast.BinaryExpression:
		e = &Expression{
			Kind: Expression_BinExpr,
			Expr: &Expression_Be{
				Be: &BinaryExpr{
					Op:    translateOp(expr.Operator.Kind),
					Left:  translateExpr(expr.Left),
					Right: translateExpr(expr.Right),
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
	case *base.Real:
		t = &Type{
			Tk:   TypeKind_REAL,
			Type: &Type_Real{Real: &Real{Name: typ.GetName()}},
		}
	case *base.Boolean:
		t = &Type{
			Tk:   TypeKind_BOOLEAN,
			Type: &Type_Bool{Bool: &Boolean{Name: typ.GetName()}},
		}
	case *base.Char:
		t = &Type{
			Tk:   TypeKind_CHAR,
			Type: &Type_Char{Char: &Char{Name: typ.GetName()}},
		}
	default:
		panic(fmt.Sprintf("Unimplemented %v", typ))
	}

	return t
}

func translateOp(op token.Kind) *Operator {
	switch op {
	case token.GreaterThan:
		return &Operator{Op: Operator_Great}
	case token.LessThan:
		return &Operator{Op: Operator_Less}
	default:
		panic(fmt.Sprintf("Unimplemented %v", op))
	}

}

// Serialize accepts `program`, an AST created from calling `serde.AstToProtoAst`,
// and converts it into a protocol buffers binary file, to be deserialized later.
func Serialize(program *Program) error {
	out, err := proto.Marshal(program)
	if err != nil {
		return fmt.Errorf("Serialization error: %s", err.Error())
	}

	outPath := "pkg/codegen/out"
	if _, err := os.Stat(outPath); errors.Is(err, os.ErrNotExist) {
		err := os.Mkdir(outPath, 0755)
		if err != nil {
			return err
		}
	}

	fileName := fmt.Sprintf("%s/%s.bin", outPath, program.Name)
	if err := os.WriteFile(fileName, out, 0644); err != nil {
		return fmt.Errorf("Write File Error: %s ", err.Error())
	}

	return nil
}
