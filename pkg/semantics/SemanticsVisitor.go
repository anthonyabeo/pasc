package semantics

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types/base"
)

// Visitor ...
type Visitor struct {
	program     *ast.Program
	symbolTable *WonkySymbolTable
}

func NewSemaVisitor(program *ast.Program, symTable *WonkySymbolTable) *Visitor {
	return &Visitor{
		program:     program,
		symbolTable: symTable,
	}
}

func (s *Visitor) VisitProgram() {
	s.VisitBlock(s.program.Block)
}

func (s *Visitor) VisitBlock(blk *ast.Block) {
	//if blk.VarDeclaration != nil {
	//	for _, varDecl := range blk.VarDeclaration.Decls {
	//		for _, id := range varDecl.Names {
	//			if s.symbolTable.DeclaredLocally(id.Name) {
	//				panic(fmt.Sprintf("symbol '%v' already defined as type '%v'", id.Name, varDecl.Type.GetName()))
	//			}
	//
	//			s.symbolTable.EnterSymbol(id.Name, NewVariable(id.Name, VARIABLE, varDecl.Type))
	//		}
	//	}
	//}
	//
	//if blk.Consts != nil {
	//	for _, constDef := range blk.Consts.Consts {
	//		if s.symbolTable.DeclaredLocally(constDef.Name.Name) {
	//			panic(fmt.Sprintf("symbol '%v' already defined as type '%v'", constDef.Name.Name, constDef.Value.Type()))
	//		}
	//
	//		s.symbolTable.EnterSymbol(
	//			constDef.Name.Name, NewConst(constDef.Name.Name, CONST, constDef.Value.Type(), constDef.Value))
	//	}
	//}

	if blk.Stats != nil {
		for _, stmt := range blk.Stats {
			stmt.Accept(s)
		}
	}
}

// VisitIdentifier ...
func (s *Visitor) VisitIdentifier(id *ast.Identifier) {
	sym := s.symbolTable.RetrieveSymbol(id.Name)
	if sym == nil {
		panic("not such symbol declared")
	}

	if !s.isRValue(sym) {
		panic("")
	}

	id.EType = sym.Type()
}

func (s *Visitor) isRValue(sym Symbol) bool {
	return sym.Kind() == CONST ||
		sym.Kind() == VARIABLE ||
		sym.Kind() == FIELD ||
		sym.Kind() == FUNCTION
}

// VisitUIntLiteral ...
func (s *Visitor) VisitUIntLiteral(i *ast.UIntegerLiteral) {
	i.EType = base.NewInteger()
}

// VisitAssignStmt ...
func (s *Visitor) VisitAssignStmt(a *ast.AssignStatement) {
	v := &LValueVisitor{
		Visitor: Visitor{
			program:     s.program,
			symbolTable: s.symbolTable,
		},
	}

	a.Variable.Accept(v)
	a.Value.Accept(s)
	if !AreAssignmentCompatible(a.Variable.Type(), a.Value.Type()) {
		panic(fmt.Sprintf("cannot assign '%s' (of type %s) to '%s' (of type %s)",
			a.Value, a.Value.Type(), a.Variable, a.Variable.Type()))
	}
}

// VisitIndexedVariable ...
func (s *Visitor) VisitIndexedVariable(iv *ast.IndexedVariable) {

}

func (s *Visitor) VisitBinaryExpr(b *ast.BinaryExpression) {
	b.Left.Accept(s)
	b.Right.Accept(s)

	lhsType := b.Left.Type().Name()
	rhsType := b.Right.Type().Name()

	switch b.Operator.Kind {
	case token.Plus, token.Minus, token.Star:
		if lhsType != "integer" && lhsType != "real" && lhsType != "set" {
			panic(fmt.Sprintf("%s must be integer, real or set type", b.Left))
		}

		if rhsType != "integer" && rhsType != "real" && rhsType != "set" {
			panic(fmt.Sprintf("%s must be integer, real or set type", b.Right))
		}

		if lhsType == "set" && rhsType == "set" {
			b.EType = b.Left.Type()
		} else if lhsType == "integer" && rhsType == "integer" {
			b.EType = b.Left.Type()
		} else {
			b.EType = base.NewReal()
		}
	case token.FwdSlash:
		if lhsType != "integer" && lhsType != "real" {
			panic(fmt.Sprintf("%s must be integer or real type", b.Left))
		}

		if rhsType != "integer" && rhsType != "real" {
			panic(fmt.Sprintf("%s must be integer or real type", b.Right))
		}

		b.EType = base.NewReal()
	case token.Div, token.Mod:
		if lhsType != "integer" && rhsType != "integer" {
			panic("both operands must be integer-type for 'mod' or 'div' expression")
		}

		b.EType = b.Left.Type()
	case token.Equal, token.LessThanGreaterThan:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" && lhsType != "pointer" && lhsType != "set" {
			panic(fmt.Sprintf("%s must be a simple-type, string-type, pointer-type or set", rhsType))
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" && rhsType != "pointer" && rhsType != "set" {
			panic(fmt.Sprintf("%s must be a simple-type, string-type, pointer-type or set", rhsType))
		}

		b.EType = base.NewBoolean()
	case token.LessThan, token.GreaterThan:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" {
			panic(fmt.Sprintf("%s must be a simple-type or string-type", lhsType))
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" {
			panic(fmt.Sprintf("%s must be a simple-type or string-type", rhsType))
		}

		b.EType = base.NewBoolean()
	case token.LessThanOrEqual, token.GreaterThanOrEqual:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" && lhsType != "set" {
			panic(fmt.Sprintf("%s must be a simple-type, string or set", rhsType))
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" && rhsType != "set" {
			panic(fmt.Sprintf("%s must be a simple-type, string or set", lhsType))
		}

		b.EType = base.NewBoolean()
	case token.In:
		if !IsOrdinalType(b.Left.Type()) {
			panic(fmt.Sprintf("cannot use %s, a non-ordinal type in an 'in' expression", lhsType))
		}

		if rhsType != "set" {
			panic(fmt.Sprintf("%s is not an appropriate RHS of an 'in' expression.", rhsType))
		}

		b.EType = base.NewBoolean()
	case token.And, token.Or:
		if lhsType != "Boolean" && rhsType != "Boolean" {
			panic(fmt.Sprintf("both operands must evaluate to 'Boolean' type"))
		}

		b.EType = base.NewBoolean()
	}
}

func (s *Visitor) VisitUnaryExpr(u *ast.UnaryExpression) {
	u.Operand.Accept(s)
	switch u.Operator.Kind {
	case token.Minus, token.Plus:
		u.EType = u.Operand.Type()
	case token.Not:
		u.EType = base.NewBoolean()
	}
}

func (s *Visitor) VisitBoolLiteral(b *ast.BoolLiteral) {
	b.EType = base.NewBoolean()
}

func (s *Visitor) VisitURealLiteral(ur *ast.URealLiteral) {
	ur.EType = base.NewReal()
}

func (s *Visitor) VisitForStatement(f *ast.ForStatement) {

}

func (s *Visitor) VisitIfStatement(i *ast.IfStatement) {

}

func (s *Visitor) VisitFuncDesignator(f *ast.FuncDesignator) {
	for _, arg := range f.Parameters {
		arg.Accept(s)
	}

	function := s.symbolTable.RetrieveSymbol(f.Name.Name)
	if function == nil {
		panic(fmt.Sprintf("undeclared symbol %v", f.Name.Name))
	} else if function.Kind() != FUNCTION {
		panic(fmt.Sprintf("attempting to call %s, which is not a function", f.Name.Name))
	} else {
		fHead := function.Type().(*ast.FuncHeading)
		f.EType = fHead.ReturnType
	}
}

func (s *Visitor) VisitGotoStatement(g *ast.GotoStatement) {

}

func (s *Visitor) VisitIdentifiedVariable(i *ast.IdentifiedVariable) {

}

func (s *Visitor) VisitWhileStatement(w *ast.WhileStatement) {

}

func (s *Visitor) VisitWithStatement(w *ast.WithStatement) {

}

func (s *Visitor) VisitRepeatStatement(r *ast.RepeatStatement) {

}

func (s *Visitor) VisitReturnStatement(r *ast.ReturnStatement) {

}

func (s *Visitor) VisitFieldDesignator(f *ast.FieldDesignator) {

}

func (s *Visitor) VisitRange(r *ast.Range) {

}

func (s *Visitor) VisitCompoundStatement(cs *ast.CompoundStatement) {

}

func (s *Visitor) VisitStrLiteral(str *ast.StrLiteral) {

}

func (s *Visitor) VisitFuncDeclaration(f *ast.FuncDeclaration) {

}

func (s *Visitor) VisitProcedureDecl(p *ast.ProcedureDeclaration) {

}

func (s *Visitor) VisitRead(r *ast.Read) {

}

func (s *Visitor) VisitReadLn(r *ast.ReadLn) {

}

func (s *Visitor) VisitWrite(w *ast.Write) {

}

func (s *Visitor) VisitWriteln(w *ast.Writeln) {

}

func (s *Visitor) VisitProcedureStmt(p *ast.ProcedureStmt) {

}

func (s *Visitor) VisitNil(n *ast.NilValue) {

}

func (s *Visitor) VisitCaseStatement(c *ast.CaseStatement) {

}

func (s *Visitor) VisitWriteParameter(w *ast.WriteParameter) {

}

func (s *Visitor) VisitSetConstructor(st *ast.SetConstructor) {

}
