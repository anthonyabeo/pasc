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
				block.VarDecl = append(block.VarDecl, v)
			}
		}
	}

	if blk.Callables != nil {
		var c *Callable

		for _, call := range blk.Callables {
			switch call := call.(type) {
			case *ast.FuncDeclaration:
				c = &Callable{
					Kind: Callable_Func,
					Call: &Callable_FuncDecl{
						FuncDecl: &FuncDeclaration{
							FuncHeading: translateFuncHeading(call.Heading),
							Blk:         translateBlock(call.Block),
						},
					},
				}
			case *ast.ProcedureDeclaration:
				c = &Callable{
					Kind: Callable_Proc,
					Call: &Callable_ProcDecl{
						ProcDecl: &ProcDeclaration{
							ProcHead: translateProcHeading(call.Heading),
							Blk:      translateBlock(call.Block),
						},
					},
				}
			default:
				panic(fmt.Sprintf("Unimplemented %v", call))
			}

			block.Callables = append(block.Callables, c)
		}
	}

	if blk.Stats != nil {
		for _, stmt := range blk.Stats {
			block.Stmts = append(block.Stmts, translateStmt(stmt))
		}
	}

	return block
}

func translateFuncHeading(fh *ast.FuncHeading) *FuncHeading {
	head := &FuncHeading{Name: fh.Name.Name, ReturnType: translateType(fh.ReturnType)}
	for _, param := range fh.Parameters {
		head.Params = append(head.Params, translateFormalParam(param))
	}

	return head
}

func translateProcHeading(ph *ast.ProcedureHeading) *ProcHeading {
	head := &ProcHeading{Name: ph.Name.Name}
	for _, param := range ph.Parameters {
		head.Params = append(head.Params, translateFormalParam(param))
	}

	return head
}

func translateFormalParam(fp ast.FormalParameter) *FormalParameter {
	var p *FormalParameter

	switch fp := fp.(type) {
	case *ast.ValueParam:
		var names []string
		for _, name := range fp.Names {
			names = append(names, name.Name)
		}

		p = &FormalParameter{
			Kind: FormalParameter_ValueParam,
			Fp: &FormalParameter_ValParam{
				ValParam: &ValueParam{
					Names: names,
					Type:  translateType(fp.Type),
				},
			},
		}
	case *ast.VariableParam:
		var names []string
		for _, name := range fp.Names {
			names = append(names, name.Name)
		}

		p = &FormalParameter{
			Kind: FormalParameter_VarParam,
			Fp: &FormalParameter_VParam{
				VParam: &VariableParam{
					Names: names,
					Type:  translateType(fp.Type),
				},
			},
		}
	case *ast.FuncHeading:
		p = &FormalParameter{
			Kind: FormalParameter_FuncHead,
			Fp: &FormalParameter_FHead{
				FHead: translateFuncHeading(fp),
			},
		}
	case *ast.ProcedureHeading:
		p = &FormalParameter{
			Kind: FormalParameter_ProcHead,
			Fp: &FormalParameter_PHead{
				PHead: translateProcHeading(fp),
			},
		}
	default:
		panic(fmt.Sprintf("Unimplemented %v", fp))
	}

	return p
}

func translateStmt(stmt ast.Statement) *Statement {
	var s *Statement

	switch stmt := stmt.(type) {
	case *ast.AssignStatement:
		s = &Statement{
			Kind: Statement_assign,
			Stmt: &Statement_AssignStmt{
				AssignStmt: &AssignStatement{
					Variable: translateExpr(stmt.Variable),
					Value:    translateExpr(stmt.Value),
				},
			},
		}
	case *ast.ProcedureStmt:
		var args []*Expression
		for _, e := range stmt.ParamList {
			args = append(args, translateExpr(e))
		}

		s = &Statement{
			Kind: Statement_procedure,
			Stmt: &Statement_ProcStmt{
				ProcStmt: &ProcedureStatement{
					Kind: ProcedureStatement_procStmt,
					Stmt: &ProcedureStatement_Ps{
						Ps: &ProcedureStatement_ProcStmt{
							Name:   translateExpr(stmt.Name),
							Params: args,
						},
					},
				},
			},
		}
	case *ast.Writeln:
		var args []*Expression
		for _, e := range stmt.ParamList {
			args = append(args, translateExpr(e))
		}

		s = &Statement{
			Kind: Statement_procedure,
			Stmt: &Statement_ProcStmt{
				ProcStmt: &ProcedureStatement{
					Kind: ProcedureStatement_wln,
					Stmt: &ProcedureStatement_WrtLn{
						WrtLn: &ProcedureStatement_WriteLn{
							Name:   stmt.Name,
							Params: args,
							File:   translateExpr(stmt.File),
						},
					},
				},
			},
		}
	case *ast.IfStatement:
		ifStatement := &IfStatement{
			Cond:     translateExpr(stmt.BoolExpr),
			TruePath: translateStmt(stmt.TruePath),
		}

		if stmt.ElsePath != nil {
			ifStatement.ElsePath = translateStmt(stmt.ElsePath)
		}

		s = &Statement{
			Kind: Statement_if,
			Stmt: &Statement_IfStmt{
				IfStmt: ifStatement,
			},
		}
	case *ast.ReturnStatement:
		s = &Statement{
			Kind: Statement_return,
			Stmt: &Statement_RetStmt{
				RetStmt: &ReturnStatement{
					Value: translateExpr(stmt.Expr),
				},
			},
		}
	case *ast.WhileStatement:
		s = &Statement{
			Kind: Statement_while,
			Stmt: &Statement_WhileStmt{
				WhileStmt: &WhileStatement{
					Cond: translateExpr(stmt.BoolExpr),
					Body: translateStmt(stmt.Body),
				},
			},
		}
	case *ast.ForStatement:
		var dir TokenKind
		if stmt.Direction == token.DownTo {
			dir = TokenKind_DOWN_TO
		} else if stmt.Direction == token.To {
			dir = TokenKind_TO
		} else {
			panic(fmt.Sprintf("Invalid for-loop direction %v", stmt.Direction))
		}

		s = &Statement{
			Kind: Statement_for,
			Stmt: &Statement_ForStmt{
				ForStmt: &ForStatement{
					CtlVar:     translateExpr(stmt.CtrlID),
					InitValue:  translateExpr(stmt.InitValue),
					FinalValue: translateExpr(stmt.FinalValue),
					Body:       translateStmt(stmt.Body),
					Dir:        dir,
				},
			},
		}
	case *ast.RepeatStatement:
		var stmts []*Statement
		for _, st := range stmt.StmtSeq {
			stmts = append(stmts, translateStmt(st))
		}

		s = &Statement{
			Kind: Statement_repeat,
			Stmt: &Statement_RptStmt{
				RptStmt: &RepeatStatement{
					Stmts: stmts,
					Cond:  translateExpr(stmt.BoolExpr),
				},
			},
		}
	case *ast.CompoundStatement:
		var stmts []*Statement
		for _, st := range stmt.Statements {
			stmts = append(stmts, translateStmt(st))
		}

		s = &Statement{
			Kind: Statement_compound,
			Stmt: &Statement_CmpdStmt{
				CmpdStmt: &CompoundStatement{
					Stmts: stmts,
				},
			},
		}
	case *ast.CaseStatement:
		var caseElems []*CaseStatement_CaseListElement
		for _, caseElem := range stmt.List {
			var constants []*Expression
			for _, constant := range caseElem.ConstList {
				constants = append(constants, translateExpr(constant))
			}

			caseElems = append(caseElems,
				&CaseStatement_CaseListElement{
					Constants: constants,
					Stmt:      translateStmt(caseElem.Body),
				})
		}

		s = &Statement{
			Kind: Statement_case,
			Stmt: &Statement_CaseStmt{
				CaseStmt: &CaseStatement{
					CaseIndex: translateExpr(stmt.Index),
					Cle:       caseElems,
				},
			},
		}
	case *ast.GotoStatement:
		v, err := strconv.Atoi(stmt.Label.Value)
		if err != nil {
			panic(err)
		}

		s = &Statement{
			Kind: Statement_goto,
			Stmt: &Statement_GotoStmt{
				GotoStmt: &GoToStatement{Label: uint32(v)}},
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
						Var: &Identifier_Variable{Name: expr.Name},
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
					Value: uint32(v),
				},
			},
		}

	case *ast.URealLiteral:
		v, err := strconv.ParseFloat(expr.Value, 64)
		if err != nil {
			panic(err)
		}

		e = &Expression{
			Kind: Expression_UReal,
			Expr: &Expression_Ureal{
				Ureal: &URealLiteral{Value: v}},
		}
	case *ast.BoolLiteral:
		v := false
		if expr.Value() == "true" {
			v = true
		}

		e = &Expression{
			Kind: Expression_Bool,
			Expr: &Expression_Bl{Bl: &BoolLiteral{Value: v}},
		}
	case *ast.StringLiteral:
	case *ast.CharString:
	case *ast.IdentifiedVariable:
	case *ast.NilValue:
		e = &Expression{
			Kind: Expression_Nil,
			Expr: &Expression_Nl{
				Nl: &NilValue{Name: "nil"}}}
	case *ast.UnaryExpression:
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
	case *ast.WriteParameter:
		wp := &WriteParameter{E: translateExpr(expr.E)}
		if expr.TotalWidth != nil {
			wp.TotalWidth = translateExpr(expr.TotalWidth)
		}

		if expr.FracDigits != nil {
			wp.FracDigits = translateExpr(expr.FracDigits)
		}

		e = &Expression{
			Kind: Expression_WriteParam,
			Expr: &Expression_Wp{Wp: wp},
		}
	case *ast.FuncDesignator:
		var args []*Expression
		for _, exp := range expr.Parameters {
			args = append(args, translateExpr(exp))
		}

		typ := expr.Scope.Resolve(expr.Name.Name).GetType()
		e = &Expression{
			Kind: Expression_FCall,
			Expr: &Expression_Fc{
				Fc: &FuncCall{
					Name:       translateExpr(expr.Name),
					Args:       args,
					ReturnType: translateType(typ),
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
			Tk:   Type_INTEGER,
			Type: &Type_Int{Int: &Type_Integer{Name: typ.GetName()}},
		}
	case *base.Real:
		t = &Type{
			Tk: Type_REAL,
			Type: &Type_Real_{Real: &Type_Real{
				Name: typ.GetName(),
			}},
		}
	case *base.Boolean:
		t = &Type{
			Tk:   Type_BOOLEAN,
			Type: &Type_Bool{Bool: &Type_Boolean{Name: typ.GetName()}},
		}
	case *base.Char:
		t = &Type{
			Tk:   Type_CHAR,
			Type: &Type_Char_{Char: &Type_Char{Name: typ.GetName()}},
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
	case token.GreaterThanOrEqual:
		return &Operator{Op: Operator_GreatEqual}
	case token.Plus:
		return &Operator{Op: Operator_Plus}
	case token.Minus:
		return &Operator{Op: Operator_Minus}
	case token.LessThanOrEqual:
		return &Operator{Op: Operator_LessEqual}
	case token.LessThanGreaterThan:
		return &Operator{Op: Operator_LessGreat}
	case token.Mod:
		return &Operator{Op: Operator_Mod}
	case token.And:
		return &Operator{Op: Operator_And}
	case token.Or:
		return &Operator{Op: Operator_Or}
	case token.Div:
		return &Operator{Op: Operator_Div}
	case token.In:
		return &Operator{Op: Operator_In}
	case token.Equal:
		return &Operator{Op: Operator_Equal}
	default:
		panic(fmt.Sprintf("Unimplemented %v", op))
	}

}

// Serialize accepts `program`, an AST created from calling `serde.AstToProtoAst`,
// and converts it into a protocol buffers binary file, to be deserialized later.
func Serialize(program *Program) error {
	out, err := proto.Marshal(program)
	if err != nil {
		return fmt.Errorf("serialization error: %s", err.Error())
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
