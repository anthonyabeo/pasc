package codegen

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/llir/llvm/ir"
	"github.com/llir/llvm/ir/constant"
	"github.com/llir/llvm/ir/enum"
	llvmTypes "github.com/llir/llvm/ir/types"
	"github.com/llir/llvm/ir/value"
)

// LLVMIRCodeGenerator ...
type LLVMIRCodeGenerator struct {
	Program  *ast.ProgramAST
	Module   *ir.Module
	symtable SymbolTable
	curBlock *ir.Block
}

// NewLLVMIRCodeGenerator creates and returns an instance of LLVMIRCodeGenerator
//
// It accepts the program's (annotated) Abstract Syntax Tree (AST) as argument and
// initializes the 'Program' field of LLVMIRCodeGenerator. The other field is of
// type LLVM IR Module.
func NewLLVMIRCodeGenerator(program *ast.ProgramAST) *LLVMIRCodeGenerator {
	return &LLVMIRCodeGenerator{
		Program:  program,
		Module:   ir.NewModule(),
		symtable: *NewSymbolTable("main", nil)}
}

// CodeGen ...
func (l *LLVMIRCodeGenerator) CodeGen() error {
	main := l.Module.NewFunc("main", llvmTypes.I32)
	l.curBlock = main.NewBlock("entry")

	for _, varDecl := range l.Program.Block.VarDeclaration.Decls {
		for _, varName := range varDecl.Names {
			elemType := l.getLLVMIRTypeFor(varDecl.Type)
			l.symtable.Define(varName.Name, l.curBlock.NewAlloca(elemType))
		}
	}

	for _, stmt := range l.Program.Block.Stats {
		if err := l.codeGenStmt(stmt); err != nil {
			return err
		}
	}

	l.curBlock.NewRet(constant.NewInt(llvmTypes.I32, 0))

	return nil
}

func (l *LLVMIRCodeGenerator) codeGenStmt(stmt ast.Statement) error {
	switch node := stmt.(type) {
	case *ast.AssignStatement:
		variable, err := l.codeGenExpr(node.Variable)
		if err != nil {
			return err
		}

		val, err := l.codeGenExpr(node.Value)
		if err != nil {
			return err
		}

		l.curBlock.NewStore(val, variable)

	case *ast.IfStatement:
	case *ast.ForStatement:
	case *ast.WhileStatement:
	case *ast.WithStatement:
	case *ast.RepeatStatement:
	case *ast.GotoStatement:
	case *ast.CaseStatement:
	case *ast.ProcedureStatement:
		if node.ProcedureID.Name == "writeln" {
			fstr := constant.NewCharArray([]byte("%d\n"))

			gFmat := l.Module.NewGlobalDef("fstr", fstr)
			gFmat.Immutable = true
			gFmat.Linkage = enum.LinkagePrivate

			printf := l.Module.NewFunc("printf", llvmTypes.I32, []*ir.Param{ir.NewParam("", llvmTypes.I8Ptr)}...)

			name := node.ParamList[0].String()
			alloca := l.symtable.Resolve(name)
			if alloca == nil {
				return fmt.Errorf("undefined variable %v", name)
			}

			gep := l.curBlock.NewGetElementPtr(fstr.Typ, gFmat, constant.NewInt(llvmTypes.I64, 0))
			val := l.curBlock.NewLoad(alloca.ElemType, alloca)

			l.curBlock.NewCall(printf, gep, val)

			return nil
		}
	}

	return nil
}

func (l *LLVMIRCodeGenerator) codeGenExpr(expr ast.Expression) (value.Value, error) {
	switch expr := expr.(type) {
	case *ast.URealLiteral:
		val, err := strconv.ParseFloat(expr.Value, 32)
		if err != nil {
			return nil, err
		}

		return constant.NewFloat(llvmTypes.Double, val), nil

	case *ast.UIntegerLiteral:
		val, err := strconv.Atoi(expr.Value)
		if err != nil {
			return nil, err
		}

		return constant.NewInt(llvmTypes.I32, int64(val)), nil

	case *ast.Identifier:
		alloca := l.symtable.Resolve(expr.Name)
		if alloca == nil {
			return nil, fmt.Errorf("undefined variable %v", expr.Name)
		}

		return alloca, nil
	}

	return nil, fmt.Errorf("codegen not implemented for expression %v", expr.String())
}

func (l *LLVMIRCodeGenerator) getLLVMIRTypeFor(t types.Type) llvmTypes.Type {
	switch t.(type) {
	case *base.Integer:
		return llvmTypes.I32
	}

	return nil
}
