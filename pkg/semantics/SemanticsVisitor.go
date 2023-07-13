package semantics

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
	"github.com/anthonyabeo/pasc/pkg/types/structured"
)

// Visitor ...
type Visitor struct {
	program     *ast.Program
	symbolTable *WonkySymbolTable
}

func NewSemaVisitor(program *ast.Program, symTable *WonkySymbolTable) *Visitor {
	return &Visitor{program: program, symbolTable: symTable}
}

func (s *Visitor) VisitProgram() error {
	return s.VisitBlock(s.program.Block)
}

func (s *Visitor) VisitBlock(blk *ast.Block) error {
	var err error

	if blk.VarDeclaration != nil {
		for _, varDecl := range blk.VarDeclaration.Decls {
			for _, id := range varDecl.Names {
				if !s.symbolTable.DeclaredLocally(id.Name) {
					s.symbolTable.EnterSymbol(id.Name, NewVariable(id.Name, VARIABLE, varDecl.Type))
				}
			}
		}
	}

	if blk.Consts != nil {
		for _, constDef := range blk.Consts.Consts {
			if !s.symbolTable.DeclaredLocally(constDef.Name.Name) {
				s.symbolTable.EnterSymbol(
					constDef.Name.Name, NewConst(constDef.Name.Name, CONST, constDef.Value.Type(), constDef.Value))
			}
		}
	}

	for _, call := range blk.Callables {
		if err = call.Accept(s); err != nil {
			return err
		}
	}

	if blk.Stats != nil {
		for _, stmt := range blk.Stats {
			if err = stmt.Accept(s); err != nil {
				return err
			}
		}
	}

	return nil
}

// VisitIdentifier ...
func (s *Visitor) VisitIdentifier(id *ast.Identifier) error {
	sym := s.symbolTable.RetrieveSymbol(id.Name)
	if sym == nil {
		return fmt.Errorf("%s is not declared", id.Name)
	}

	if !s.isRValue(sym) {
		return fmt.Errorf("%s is not an RValue", sym.Name())
	}

	id.EType = sym.Type()

	return nil
}

func (s *Visitor) isRValue(sym Symbol) bool {
	return sym.Kind() == CONST ||
		sym.Kind() == VARIABLE ||
		sym.Kind() == FIELD ||
		sym.Kind() == FUNCTION
}

// VisitUIntLiteral ...
func (s *Visitor) VisitUIntLiteral(i *ast.UIntegerLiteral) error {
	i.EType = base.NewInteger()
	return nil
}

// VisitAssignStmt ...
func (s *Visitor) VisitAssignStmt(a *ast.AssignStatement) error {
	var err error

	v := &LValueVisitor{
		Visitor: Visitor{
			program:     s.program,
			symbolTable: s.symbolTable,
		},
	}

	if err = a.Variable.Accept(v); err != nil {
		return err
	}

	if err = a.Value.Accept(s); err != nil {
		return err
	}

	if !AreAssignmentCompatible(a.Variable.Type(), a.Value.Type()) {
		return fmt.Errorf("cannot assign '%s' (of type %s) to '%s' (of type %s)",
			a.Value, a.Value.Type(), a.Variable, a.Variable.Type())
	}

	return nil
}

// VisitIndexedVariable ...
func (s *Visitor) VisitIndexedVariable(iv *ast.IndexedVariable) error {
	var err error

	for _, idx := range iv.IndexExpr {
		if err = idx.Accept(s); err != nil {
			return err
		}

		if _, ok := idx.Type().(types.Ordinal); !ok {
			return fmt.Errorf(
				"'%s' (of type '%s') cannot be used as index type in array %s", idx, idx.Type(), iv)
		}
	}

	array := s.symbolTable.RetrieveSymbol(iv.ArrayVar.String()).Type()
	for i := 0; i < len(iv.IndexExpr); i++ {
		arr := array.(*types.Array)
		if arr == nil {
			return fmt.Errorf("%s is undefined", iv.ArrayVar.String())
		} else if arr.Name() != "array" {
			return fmt.Errorf("cannot index into non-array type, %s", iv.ArrayVar)
		} else {
			array = arr.ComponentType
		}
	}

	iv.EType = array

	return nil
}

func (s *Visitor) VisitBinaryExpr(b *ast.BinaryExpression) error {
	var err error

	if err = b.Left.Accept(s); err != nil {
		return err
	}

	if err = b.Right.Accept(s); err != nil {
		return err
	}

	lhsType := b.Left.Type().Name()
	rhsType := b.Right.Type().Name()

	switch b.Operator {
	case token.Plus, token.Minus, token.Star:
		if lhsType != "integer" && lhsType != "real" && lhsType != "set" {
			return fmt.Errorf("%s must be integer, real or set type", b.Left)
		}

		if rhsType != "integer" && rhsType != "real" && rhsType != "set" {
			return fmt.Errorf("%s must be integer, real or set type", b.Right)
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
			return fmt.Errorf("%s must be integer or real type", b.Left)
		}

		if rhsType != "integer" && rhsType != "real" {
			return fmt.Errorf("%s must be integer or real type", b.Right)
		}

		b.EType = base.NewReal()
	case token.Div, token.Mod:
		if lhsType != "integer" && rhsType != "integer" {
			return fmt.Errorf("both operands must be integer-type for 'mod' or 'div' expression. "+
				"got %s and %s instead", lhsType, rhsType)
		}

		b.EType = b.Left.Type()
	case token.Equal, token.LessThanGreaterThan:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" && lhsType != "pointer" && lhsType != "set" {
			return fmt.Errorf("%s must be a simple-type, string-type, pointer-type or set", rhsType)
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" && rhsType != "pointer" && rhsType != "set" {
			return fmt.Errorf("%s must be a simple-type, string-type, pointer-type or set", rhsType)
		}

		b.EType = base.NewBoolean()
	case token.LessThan, token.GreaterThan:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" {
			return fmt.Errorf("%s must be a simple-type or string-type", lhsType)
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" {
			return fmt.Errorf("%s must be a simple-type or string-type", rhsType)
		}

		b.EType = base.NewBoolean()
	case token.LessThanOrEqual, token.GreaterThanOrEqual:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" && lhsType != "set" {
			return fmt.Errorf("%s must be a simple-type, string or set", rhsType)
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" && rhsType != "set" {
			return fmt.Errorf("%s must be a simple-type, string or set", lhsType)
		}

		b.EType = base.NewBoolean()
	case token.In:
		if !IsOrdinalType(b.Left.Type()) {
			return fmt.Errorf("cannot use %s, a non-ordinal type in an 'in' expression", lhsType)
		}

		if rhsType != "set" {
			return fmt.Errorf("%s is not an appropriate RHS of an 'in' expression", rhsType)
		}

		b.EType = base.NewBoolean()
	case token.And, token.Or:
		if lhsType != "Boolean" && rhsType != "Boolean" {
			return fmt.Errorf("'%s' and '%s' must evaluate to 'Boolean' type", lhsType, rhsType)
		}

		b.EType = base.NewBoolean()
	}

	return nil
}

func (s *Visitor) VisitUnaryExpr(u *ast.UnaryExpression) error {
	if err := u.Operand.Accept(s); err != nil {
		return err
	}

	switch u.Operator {
	case token.Minus, token.Plus:
		u.EType = u.Operand.Type()
	case token.Not:
		u.EType = base.NewBoolean()
	}

	return nil
}

func (s *Visitor) VisitBoolLiteral(b *ast.BoolLiteral) error {
	b.EType = base.NewBoolean()
	return nil
}

func (s *Visitor) VisitURealLiteral(ur *ast.URealLiteral) error {
	ur.EType = base.NewReal()
	return nil
}

func (s *Visitor) VisitForStatement(f *ast.ForStatement) error {
	var err error

	if err = f.CtrlID.Accept(s); err != nil {
		return err
	}

	if err = f.InitValue.Accept(s); err != nil {
		return err
	}

	if err = f.FinalValue.Accept(s); err != nil {
		return err
	}

	if _, ok := f.CtrlID.EType.(types.Ordinal); !ok {
		return fmt.Errorf("loop control variable '%s' must be of type, integer, char, Boolean, enum or subrange type,"+
			"currently it is a '%s' type", f.CtrlID, f.CtrlID.EType)
	}

	// check that ctrlID is compatible to initValue and finalValue
	if !AreCompatibleTypes(f.InitValue.Type(), f.CtrlID.EType) {
		return fmt.Errorf("control variable '%s' (of type '%s'), is not compatible with initial value of type '%s'",
			f.CtrlID, f.CtrlID.EType, f.InitValue.Type())
	}

	if !AreCompatibleTypes(f.FinalValue.Type(), f.CtrlID.EType) {
		return fmt.Errorf("control variable '%s' (of type '%s'), is not compatible with final value of type '%s'",
			f.CtrlID, f.CtrlID.EType, f.FinalValue.Type())
	}

	// check that the right direction is set
	if f.Direction != token.To && f.Direction != token.DownTo {
		return fmt.Errorf("invalid loop direction. use 'to' or 'downto', instead of '%s'", f.Direction)
	}

	if err = f.Body.Accept(s); err != nil {
		return err
	}

	return nil
}

func (s *Visitor) VisitIfStatement(i *ast.IfStatement) error {
	var err error

	if err = i.BoolExpr.Accept(s); err != nil {
		return err
	}

	if err = i.TruePath.Accept(s); err != nil {
		return err
	}

	if i.ElsePath != nil {
		if err = i.ElsePath.Accept(s); err != nil {
			return err
		}
	}

	if i.BoolExpr.Type().Name() != "Boolean" {
		return fmt.Errorf("if-statement condition must evaluate to a Boolean, %s does not", i.BoolExpr)
	}

	return nil
}

func (s *Visitor) VisitFuncDesignator(f *ast.FuncDesignator) error {
	for _, arg := range f.Args {
		if err := arg.Accept(s); err != nil {
			return err
		}
	}

	function := s.symbolTable.RetrieveSymbol(f.Name.Name)
	if function == nil {
		return fmt.Errorf("undeclared symbol %v", f.Name.Name)
	} else if function.Kind() != FUNCTION {
		return fmt.Errorf("attempting to call %s, which is not a function", f.Name.Name)
	} else {
		fHead := function.Type().(*ast.FuncHeading)

		numParams := 0
		for _, param := range fHead.Parameters {
			switch param := param.(type) {
			case *ast.VariableParam:
				numParams += len(param.Names)
			case *ast.ValueParam:
				numParams += len(param.Names)
			default:
				numParams += 1
			}
		}

		if numParams != len(f.Args) {
			return fmt.Errorf("not enough arguments to function call '%s'", f)
		}

		offset := 0
		for _, param := range fHead.Parameters {
			switch param := param.(type) {
			case *ast.VariableParam:
				for i, name := range param.Names {
					sym := s.symbolTable.RetrieveSymbol(f.Args[offset+i].String())
					if sym == nil || sym.Kind() != VARIABLE {
						return fmt.Errorf("argument '%s' used in funcion call '%s' must be a variable", f.Args[offset+i].String(), f)
					} else {
						if !AreAssignmentCompatible(sym.Type(), name.EType) {
							return fmt.Errorf(
								"mismatched parameters: argument at position '%d' in function call (%s) does not match "+
									"parameter at position '%d' in function '%s'", offset+i, f, offset+i, fHead)
						}
					}
				}

				offset += len(param.Names)
			case *ast.ValueParam:
				for j, name := range param.Names {
					if !AreAssignmentCompatible(f.Args[offset+j].Type(), name.EType) {
						return fmt.Errorf(
							"mismatched parameters: argument at position '%d' in function call (%s) does not match "+
								"parameter at position '%d' in function '%s'", offset+j, f, offset+j, fHead)
					}
				}

				offset += len(param.Names)
			default:
				if !AreAssignmentCompatible(f.Args[offset].Type(), param.Type()) {
					return fmt.Errorf(
						"mismatched parameters: argument at position '%d' in function call (%s) does not match "+
							"parameter at position '%d' in function '%s'", offset+1, f, offset+1, fHead)
				}

				offset++
			}
		}

		f.EType = fHead.ReturnType
	}

	return nil
}

func (s *Visitor) VisitGotoStatement(g *ast.GotoStatement) error {
	if err := g.Label.Accept(s); err != nil {
		return err
	}

	return nil
}

func (s *Visitor) VisitIdentifiedVariable(i *ast.IdentifiedVariable) error {
	return nil
}

func (s *Visitor) VisitWhileStatement(whl *ast.WhileStatement) error {
	var err error

	if err = whl.BoolExpr.Accept(s); err != nil {
		return err
	}

	if err = whl.Body.Accept(s); err != nil {
		return err
	}

	if whl.BoolExpr.Type().Name() != "Boolean" {
		return fmt.Errorf("while-statement condition must evaluate to a Boolean, '%s' does not", whl.BoolExpr)
	}

	return nil
}

func (s *Visitor) VisitWithStatement(with *ast.WithStatement) error {
	var err error

	s.symbolTable.OpenScope()

	for _, r := range with.RecordVarList {
		sym := s.symbolTable.RetrieveSymbol(r.String())
		if sym == nil {
			return fmt.Errorf("undefined name %s", r)
		} else if sym.Type().Name() != "record" {
			return fmt.Errorf("%s is not a record type", sym.Type().Name())
		} else {
			record := sym.Type().(*structured.Record)
			for _, field := range record.FieldList {
				switch fld := field.(type) {
				case *structured.FixedPart:
					offset := 0
					for _, recSec := range fld.Entry {
						for _, id := range recSec.List {
							if err := id.Accept(s); err != nil {
								return err
							}

							s.symbolTable.EnterSymbol(
								id.Name, NewField(id.Name, FIELD, id.EType, uint64(offset)))
						}
						offset++
					}
				case *structured.VariantPart:
				}
			}
		}
	}

	if err = with.Body.Accept(s); err != nil {
		return err
	}

	s.symbolTable.CloseScope()

	return nil
}

func (s *Visitor) VisitRepeatStatement(rpt *ast.RepeatStatement) error {
	var err error

	for _, stmt := range rpt.StmtSeq {
		if err = stmt.Accept(s); err != nil {
			return err
		}
	}

	if err = rpt.BoolExpr.Accept(s); err != nil {
		return err
	}

	if rpt.BoolExpr.Type().Name() != "Boolean" {
		return fmt.Errorf("repeat-statement condition must evaluate to a Boolean, %s does not", rpt.BoolExpr)
	}

	return nil
}

func (s *Visitor) VisitReturnStatement(ret *ast.ReturnStatement) error {
	if err := ret.Expr.Accept(s); err != nil {
		return err
	}

	return nil
}

func (s *Visitor) VisitFieldDesignator(f *ast.FieldDesignator) error {
	sym := s.symbolTable.RetrieveSymbol(f.RecordVar.String())
	if sym == nil {
		return fmt.Errorf("undefined name %s", f.RecordVar.String())
	} else if sym.Type().Name() != "record" {
		return fmt.Errorf("%s is not a record type", sym.Type().Name())
	} else {
		if !s.symbolTable.DeclaredLocally(f.FieldSpec.String()) {
			return fmt.Errorf("symbol %s not declared", f.FieldSpec.String())
		}

		if err := f.FieldSpec.Accept(s); err != nil {
			return err
		}

		f.EType = f.FieldSpec.Type()
	}

	return nil
}

func (s *Visitor) VisitRange(r *ast.Range) error {
	var err error

	if err = r.Start.Accept(s); err != nil {
		return err
	}

	if err = r.End.Accept(s); err != nil {
		return err
	}

	r.EType = r.Start.Type()

	return nil
}

func (s *Visitor) VisitCompoundStatement(cs *ast.CompoundStatement) error {
	for _, stmt := range cs.Statements {
		if err := stmt.Accept(s); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitStrLiteral(str *ast.StrLiteral) error {
	str.EType = base.NewString()
	return nil
}

func (s *Visitor) VisitFuncDeclaration(f *ast.FuncDeclaration) error {
	var err error

	if !s.symbolTable.DeclaredLocally(f.Heading.FName.Name) {
		s.symbolTable.EnterSymbol(f.Heading.FName.Name, NewFunction(f.Heading.FName.Name, FUNCTION, f.Heading))
	}
	s.symbolTable.OpenScope()

	if err = f.Heading.Accept(s); err != nil {
		return err
	}

	if err = s.VisitBlock(f.Block); err != nil {
		return err
	}

	for _, stmt := range f.Block.Stats {
		if retStmt, ok := stmt.(*ast.ReturnStatement); ok {
			if retStmt.Expr.Type().Name() != f.Heading.ReturnType.Name() {
				return fmt.Errorf("declared return type of %s is %s, does not match return value type %s",
					f.Heading.FName, f.Heading.ReturnType, retStmt.Expr.Type().Name())
			}
		}
	}

	s.symbolTable.CloseScope()

	return nil
}

func (s *Visitor) VisitProcedureDecl(p *ast.ProcedureDeclaration) error {
	var err error

	if !s.symbolTable.DeclaredLocally(p.Heading.PName.Name) {
		s.symbolTable.EnterSymbol(p.Heading.PName.Name, NewProcedure(p.Heading.PName.Name, PROCEDURE, p.Heading))
	}
	s.symbolTable.OpenScope()

	if err = p.Heading.Accept(s); err != nil {
		return err
	}

	if err = s.VisitBlock(p.Block); err != nil {
		return err
	}

	s.symbolTable.CloseScope()

	return nil
}

func (s *Visitor) VisitRead(r *ast.Read) error {
	return nil
}

func (s *Visitor) VisitReadLn(r *ast.ReadLn) error {
	return nil
}

func (s *Visitor) VisitWrite(w *ast.Write) error {
	return nil
}

func (s *Visitor) VisitWriteln(w *ast.Writeln) error {
	return nil
}

func (s *Visitor) VisitProcedureStmt(p *ast.ProcedureStmt) error {
	for _, param := range p.Args {
		if err := param.Accept(s); err != nil {
			return err
		}
	}

	sym := s.symbolTable.RetrieveSymbol(p.Name.Name)
	if sym == nil {
		return fmt.Errorf("name '%s' is undeclared", p.Name.Name)
	} else if sym.Kind() != PROCEDURE {
		return fmt.Errorf("cannot call '%s', it is not a procedure", p.Name.Name)
	} else {
		pHead := sym.Type().(*ast.ProcedureHeading)

		numParams := 0
		for _, param := range pHead.Parameters {
			switch param := param.(type) {
			case *ast.VariableParam:
				numParams += len(param.Names)
			case *ast.ValueParam:
				numParams += len(param.Names)
			default:
				numParams += 1
			}
		}

		if numParams != len(p.Args) {
			return fmt.Errorf("not enough arguments to procedure call '%s'", p)
		}

		offset := 0
		for _, param := range pHead.Parameters {
			switch param := param.(type) {
			case *ast.VariableParam:
				for i, name := range param.Names {
					sym := s.symbolTable.RetrieveSymbol(p.Args[offset+i].String())
					if sym == nil || sym.Kind() != VARIABLE {
						return fmt.Errorf("argument '%s' used in procedure call '%s' must be a variable", p.Args[offset+i].String(), p)
					} else {
						if !AreAssignmentCompatible(sym.Type(), name.EType) {
							return fmt.Errorf(
								"mismatched parameters: argument at position '%d' in procedure call (%s) does not match "+
									"parameter at position '%d' in procedure '%s'", offset+i, p, offset+i, pHead)
						}
					}
				}

				offset += len(param.Names)
			case *ast.ValueParam:
				for j, name := range param.Names {
					if !AreAssignmentCompatible(p.Args[offset+j].Type(), name.EType) {
						return fmt.Errorf(
							"mismatched parameters: argument at position '%d' in procedure call (%s) does not match "+
								"parameter at position '%d' in procedure '%s'", offset+j, p, offset+j, pHead)
					}
				}

				offset += len(param.Names)
			default:
				if !AreAssignmentCompatible(p.Args[offset].Type(), param.Type()) {
					return fmt.Errorf(
						"mismatched parameters: argument at position '%d' in procedure call (%s) does not match "+
							"parameter at position '%d' in procedure '%s'", offset+1, p, offset+1, pHead)
				}

				offset++
			}
		}
	}

	return nil
}

func (s *Visitor) VisitNil(n *ast.NilValue) error {
	return nil
}

func (s *Visitor) VisitCaseStatement(cse *ast.CaseStatement) error {
	var err error

	if err = cse.Index.Accept(s); err != nil {
		return err
	}

	for _, caseElem := range cse.List {
		for _, cst := range caseElem.ConstList {
			if err = cst.Accept(s); err != nil {
				return err
			}
		}

		if err = caseElem.Body.Accept(s); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitWriteParameter(w *ast.WriteParameter) error {
	return nil
}

func (s *Visitor) VisitSetConstructor(st *ast.SetConstructor) error {
	for _, m := range st.Members {
		if err := m.Accept(s); err != nil {
			return err
		}
	}

	if len(st.Members) == 0 {
		return fmt.Errorf("set-constructor has no members")
	}

	st.EType = &types.Set{
		TokenKind: token.Set,
		BaseType:  st.Members[0].Type().(types.Ordinal),
	}

	return nil
}

func (s *Visitor) VisitValueParam(v *ast.ValueParam) error {
	for _, param := range v.Names {
		if s.symbolTable.DeclaredLocally(param.Name) {
			return fmt.Errorf("symbol '%s' already defined as type '%s'", param.Name, v.Typ)
		}

		s.symbolTable.EnterSymbol(param.Name, NewVariable(param.Name, VARIABLE, v.Typ))
		if err := param.Accept(s); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitVariableParam(v *ast.VariableParam) error {
	for _, param := range v.Names {
		if s.symbolTable.DeclaredLocally(param.Name) {
			return fmt.Errorf("symbol '%s' already defined as type '%s'", param.Name, v.Typ)
		}

		s.symbolTable.EnterSymbol(param.Name, NewVariable(param.Name, VARIABLE, v.Typ))

		v := &LValueVisitor{Visitor: Visitor{program: s.program, symbolTable: s.symbolTable}}
		if err := param.Accept(v); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitFuncHeading(f *ast.FuncHeading) error {
	if !s.symbolTable.DeclaredLocally(f.FName.Name) {
		s.symbolTable.EnterSymbol(f.FName.Name, NewFunction(f.FName.Name, FUNCTION, f))
	}

	for _, param := range f.Parameters {
		if err := param.Accept(s); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitProcedureHeading(p *ast.ProcedureHeading) error {
	for _, param := range p.Parameters {
		if err := param.Accept(s); err != nil {
			return err
		}
	}

	return nil
}
