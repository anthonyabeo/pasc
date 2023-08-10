package semantics

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
	"github.com/anthonyabeo/pasc/pkg/types/structured"
)

// Visitor ...
type Visitor struct {
	program     *ast.Program
	symbolTable *SymbolTable
}

func NewSemaVisitor(program *ast.Program, symTable *SymbolTable) *Visitor {
	return &Visitor{program: program, symbolTable: symTable}
}

func (s *Visitor) VisitProgram() error {
	return s.VisitBlock(s.program.Block)
}

func (s *Visitor) VisitBlock(blk *ast.Block) error {
	var err error

	if blk.Labels != nil {
		if err = blk.Labels.Accept(s); err != nil {
			return err
		}
	}

	if blk.Types != nil {
		if err = blk.Types.Accept(s); err != nil {
			return err
		}
	}

	if blk.VarDeclaration != nil {
		if err := blk.VarDeclaration.Accept(s); err != nil {
			return err
		}
	}

	if blk.Consts != nil {
		if err = blk.Consts.Accept(s); err != nil {
			return err
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
		return fmt.Errorf("[Semantic Error at '%s']\n\t name '%s' is not declared", id.Pos(), id.Name)
	}

	if !s.isRValue(sym) {
		return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' is not an RValue", id.Pos(), sym.Name())
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

	if !s.AreAssignmentCompatible(a.Value, a.Variable) {
		return fmt.Errorf("[Semantic Error at '%s']\n\t cannot assign '%s' (of type %s) to '%s' (of type %s)",
			a.Pos(), a.Value, a.Value.Type(), a.Variable, a.Variable.Type())
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
				"[Semantic Error at '%s']\n\t expression '%s' (of type '%s'), cannot be used as index type in array '%s'",
				idx.Pos(), idx, idx.Type(), iv)
		}
	}

	array := s.symbolTable.RetrieveSymbol(iv.ArrayVar.String()).Type()
	for i := 0; i < len(iv.IndexExpr); i++ {
		arr := array.(*types.Array)
		if arr == nil {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' is undefined as an array", iv.Pos(), iv.ArrayVar.String())
		} else if arr.Name() != "array" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t cannot index into non-array type, %s", iv.Pos(), iv.ArrayVar)
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

	switch b.Operator.Kind {
	case token.Plus, token.Minus, token.Star:
		if lhsType != "integer" && lhsType != "real" && lhsType != "set" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be integer, real or set type",
				b.Pos(), b.Left)
		}

		if rhsType != "integer" && rhsType != "real" && rhsType != "set" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be integer, real or set type",
				b.Pos(), b.Right)
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
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be integer or real type",
				b.Pos(), b.Left)
		}

		if rhsType != "integer" && rhsType != "real" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be integer or real type",
				b.Pos(), b.Right)
		}

		b.EType = base.NewReal()
	case token.Div, token.Mod:
		if lhsType != "integer" && rhsType != "integer" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t both operands must be integer-type for 'mod' or "+
				"'div' expression. Got '%s' and '%s' instead", b.Pos(), lhsType, rhsType)
		}

		b.EType = b.Left.Type()
	case token.Equal, token.NotEqual:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" && lhsType != "pointer" && lhsType != "set" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be a simple-type, string-type, pointer-type or set",
				b.Pos(), rhsType)
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" && rhsType != "pointer" && rhsType != "set" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be a simple-type, string-type, pointer-type or set",
				b.Pos(), rhsType)
		}

		b.EType = base.NewBoolean()
	case token.LessThan, token.GreaterThan:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be a simple-type or string-type",
				b.Pos(), lhsType)
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be a simple-type or string-type",
				b.Pos(), rhsType)
		}

		b.EType = base.NewBoolean()
	case token.LessThanOrEqual, token.GreaterThanOrEqual:
		if !IsSimpleType(b.Left.Type()) && lhsType != "string" && lhsType != "set" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be a simple-type, string or set",
				b.Pos(), rhsType)
		}

		if !IsSimpleType(b.Right.Type()) && rhsType != "string" && rhsType != "set" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' must be a simple-type, string or set",
				b.Pos(), lhsType)
		}

		b.EType = base.NewBoolean()
	case token.In:
		if !IsOrdinalType(b.Left.Type()) {
			return fmt.Errorf("[Semantic Error at '%s']\n\t cannot use '%s', a non-ordinal type in an 'in' expression",
				b.Pos(), lhsType)
		}

		if rhsType != "set" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' is not an appropriate RHS of an 'in' expression",
				b.Pos(), rhsType)
		}

		b.EType = base.NewBoolean()
	case token.And, token.Or:
		if lhsType != "Boolean" && rhsType != "Boolean" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' and '%s' must evaluate to 'Boolean' type",
				b.Pos(), lhsType, rhsType)
		}

		b.EType = base.NewBoolean()
	}

	return nil
}

func (s *Visitor) VisitUnaryExpr(u *ast.UnaryExpression) error {
	if err := u.Operand.Accept(s); err != nil {
		return err
	}

	switch u.Operator.Kind {
	case token.Minus, token.Plus:
		if u.Operand.Type().Name() != "integer" && u.Operand.Type().Name() != "real" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t operand, '%s' in expression, '%s', must be real or "+
				"integer type. it is a '%s' type", u.Pos(), u.Operand, u, u.Operand.Type())
		}

		u.EType = u.Operand.Type()
	case token.Not:
		if u.Operand.Type().Name() != "Boolean" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t not-expression, '%s' operand must evaluate to a Boolean type, "+
				"it evaluates to '%s'", u.Pos(), u, u.Operand.Type())
		}

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
		return fmt.Errorf("[Semantic Error at '%s']\n\t loop control variable '%s' must be of type, integer, "+
			"char, Boolean, enum or subrange type, currently it is a '%s' type", f.CtrlID.Pos(), f.CtrlID, f.CtrlID.EType)
	}

	// check that ctrlID is compatible to initValue and finalValue
	if !AreCompatibleTypes(f.InitValue.Type(), f.CtrlID.EType) {
		return fmt.Errorf("[Semantic Error at '%s']\n\t control variable '%s' (of type '%s'), is not compatible "+
			"with initial value of type '%s'", f.CtrlID.Pos(), f.CtrlID, f.CtrlID.EType, f.InitValue.Type())
	}

	if !AreCompatibleTypes(f.FinalValue.Type(), f.CtrlID.EType) {
		return fmt.Errorf("[Semantic Error at '%s']\n\t control variable '%s' (of type '%s'), is not compatible "+
			"with final value of type '%s'", f.FinalValue.Pos(), f.CtrlID, f.CtrlID.EType, f.FinalValue.Type())
	}

	// check that the right direction is set
	if f.Direction != token.To && f.Direction != token.DownTo {
		return fmt.Errorf("[Semantic Error at '%s']\n\t invalid loop direction. use 'to' or 'downto', instead of '%s'",
			f.Pos(), f.Direction)
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
		return fmt.Errorf("[Semantic Error at '%s']\n\t if-statement condition must evaluate to a Boolean, "+
			"'%s' does not", i.BoolExpr.Pos(), i.BoolExpr)
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
		return fmt.Errorf("[Semantic Error at '%s']\n\t undeclared symbol %v", f.Pos(), f.Name.Name)
	} else if function.Kind() != FUNCTION {
		return fmt.Errorf("[Semantic Error at '%s']\n\t attempting to call %s, which is not a function",
			f.Pos(), f.Name.Name)
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
			return fmt.Errorf(
				"[Semantic Error at '%s']\n\t not enough arguments to function call '%s'", f.Pos(), f)
		}

		offset := 0
		for _, param := range fHead.Parameters {
			switch param := param.(type) {
			case *ast.VariableParam:
				for i, name := range param.Names {
					sym := s.symbolTable.RetrieveSymbol(f.Args[offset+i].String())
					if sym == nil || sym.Kind() != VARIABLE {
						return fmt.Errorf(
							"[Semantic Error at '%s']\n\t argument '%s' used in funcion call '%s' must be a variable",
							f.Args[offset+i].Pos(), f.Args[offset+i].String(), f)
					} else {
						if !s.AreAssignmentCompatible(f.Args[offset+i], name) {
							return fmt.Errorf(
								"[Semantic Error at '%s']\n\t mismatched parameters: argument at position '%d' "+
									"in function call '%s' does not match parameter at position '%d' in function '%s'",
								f.Args[offset+i].Pos(), offset+i+1, f, offset+i+1, fHead)
						}
					}
				}

				offset += len(param.Names)
			case *ast.ValueParam:
				for j, name := range param.Names {
					if !s.AreAssignmentCompatible(f.Args[offset+j], name) {
						return fmt.Errorf(
							"[Semantic Error at '%s']\n\t mismatched parameters: argument at position '%d' in "+
								"function call (%s) does not match parameter at position '%d' in function '%s'",
							f.Args[offset+j].Pos(), offset+j, f, offset+j, fHead)
					}
				}

				offset += len(param.Names)
			default:
				if !s.AreAssignmentCompatible(f.Args[offset], param) {
					return fmt.Errorf(
						"[Semantic Error at '%s']\n\t mismatched parameters: argument at position '%d' in "+
							"function call (%s) does not match parameter at position '%d' in function '%s'",
						f.Args[offset].Pos(), offset+1, f, offset+1, fHead)
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

	if g.Label.EType.Name() != "integer" {
		return fmt.Errorf(
			"[Semantic Error at '%s']\n\t label must be integer-type, got '%s' instead",
			g.Label.Pos(), g.Label.EType.Name())
	}

	val, err := strconv.Atoi(g.Label.Value)
	if err != nil || (val < 0 || val > 9999) {
		return fmt.Errorf(
			"[Semantic Error at '%s']\n\t label value, '%s' fall outside the required range [0, 9999]",
			g.Label.Pos(), g.Label.Value)
	}

	return nil
}

func (s *Visitor) VisitIdentifiedVariable(i *ast.IdentifiedVariable) error {
	sym := s.symbolTable.RetrieveSymbol(i.PointerName.Name)
	if sym == nil {
		return fmt.Errorf("[Semantic Error at '%s']\n\t the name '%s' is not declared",
			i.Pos(), i.PointerName.Name)
	} else if sym.Kind() != VARIABLE {
		return fmt.Errorf("[Semantic Error at '%s']\n\t the name '%s' is not a variable",
			i.Pos(), i.PointerName.Name)
	} else if sym.Type().Name() != "pointer" {
		return fmt.Errorf("[Semantic Error at '%s']\n\t the name '%s' is not a pointer type",
			i.Pos(), i.PointerName.Name)
	} else {
		ptr := sym.Type().(*structured.Pointer)
		if ptr.DomainType.String() != i.UnderType.String() {
			return fmt.Errorf("[Semantic Error at '%s']\n\t name '%s', is declared as a pointer to '%s', "+
				"but it points to '%s' instead", i.Pos(), i.PointerName.Name, ptr.DomainType.String(), i.UnderType.String())
		}

		i.EType = ptr
	}

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
		return fmt.Errorf(
			"[Semantic Error at '%s']\n\t while-statement condition must evaluate to a Boolean, '%s' does not",
			whl.BoolExpr.Pos(), whl.BoolExpr)
	}

	return nil
}

func (s *Visitor) VisitWithStatement(with *ast.WithStatement) error {
	var err error

	s.symbolTable.OpenScope()

	for _, r := range with.RecordVarList {
		sym := s.symbolTable.RetrieveSymbol(r.String())
		if sym == nil {
			return fmt.Errorf("[Semantic Error at '%s']\n\t undefined name %s", r.Pos(), r)
		} else if sym.Type().Name() != "record" {
			return fmt.Errorf("[Semantic Error at '%s']\n\t '%s' is not a record type", r.Pos(), sym.Type().Name())
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
		return fmt.Errorf(
			"[Semantic Error at '%s']\n\t repeat-statement condition must evaluate to a Boolean, '%s' does not",
			rpt.BoolExpr.Pos(), rpt.BoolExpr)
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
		return fmt.Errorf(
			"[Semantic Error at '%s']\n\t undefined name %s", f.RecordVar.Pos(), f.RecordVar.String())
	} else if sym.Type().Name() != "record" {
		return fmt.Errorf(
			"[Semantic Error at '%s']\n\t '%s' is not a record type", f.RecordVar.Pos(), sym.Type().Name())
	} else {
		if !s.symbolTable.DeclaredLocally(f.FieldSpec.String()) {
			return fmt.Errorf(
				"[Semantic Error at '%s']\n\t symbol %s not declared", f.FieldSpec.Pos(), f.FieldSpec.String())
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

	if _, ok := r.Start.Type().(types.Ordinal); !ok {
		return fmt.Errorf("[Semantic Error at '%s']\n\t start constant of range '%s' must be of type, integer, "+
			"char, Boolean, enum or subrange type,currently it is a '%s' type", r.Start.Pos(), r.Start, r.Start.Type())
	}

	if _, ok := r.End.Type().(types.Ordinal); !ok {
		return fmt.Errorf("[Semantic Error at '%s']\n\t end constant of range '%s' must be of type, integer, "+
			"char, Boolean, enum or subrange type,currently it is a '%s' type", r.End.Pos(), r.End, r.End.Type())
	}

	if r.Start.Type().Name() != r.End.Type().Name() {
		return fmt.Errorf("[Semantic Error at '%s']\n\t start constant (%s) and end constant (%s) in range (%s) "+
			"are not the same type", r.Pos(), r.Start.Type().Name(), r.End.Type().Name(), r)
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
	str.EType = base.NewString(len(str.Value))
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
				return fmt.Errorf(
					"[Semantic Error at '%s']\n\t declared return type of %s is %s, does not match return value "+
						"type %s", f.Heading.Pos(), f.Heading.FName, f.Heading.ReturnType, retStmt.Expr.Type().Name())
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
	for _, param := range r.VarAccess {
		if err := param.Accept(s); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitReadLn(r *ast.ReadLn) error {
	for _, param := range r.VarAccess {
		if err := param.Accept(s); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitWrite(w *ast.Write) error {
	for _, param := range w.ParamList {
		if err := param.Accept(s); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitWriteln(w *ast.Writeln) error {
	for _, param := range w.ParamList {
		if err := param.Accept(s); err != nil {
			return err
		}
	}

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
		return fmt.Errorf("[Semantic Error at '%s']\n\t name '%s' is undeclared", p.Name.Pos(), p.Name.Name)
	} else if sym.Kind() != PROCEDURE {
		return fmt.Errorf("[Semantic Error at '%s']\n\t cannot call '%s', it is not a procedure", p.Name.Pos(), p.Name.Name)
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
			return fmt.Errorf(
				"[Semantic Error at '%s']\n\t not enough arguments to procedure call '%s'", p.Pos(), p)
		}

		offset := 0
		for _, param := range pHead.Parameters {
			switch param := param.(type) {
			case *ast.VariableParam:
				for i, name := range param.Names {
					sym := s.symbolTable.RetrieveSymbol(p.Args[offset+i].String())
					if sym == nil || sym.Kind() != VARIABLE {
						return fmt.Errorf(
							"[Semantic Error at '%s']\n\t argument '%s' used in procedure call '%s' must be a "+
								"variable", p.Args[offset+i].Pos(), p.Args[offset+i].String(), p)
					} else {
						if !s.AreAssignmentCompatible(p.Args[offset+i], name) {
							return fmt.Errorf(
								"[Semantic Error at '%s']\n\t mismatched parameters: argument at position '%d' "+
									"in procedure call (%s) does not match parameter at position '%d' in procedure '%s'",
								p.Args[offset+i].Pos(), offset+i, p, offset+i, pHead)
						}
					}
				}

				offset += len(param.Names)
			case *ast.ValueParam:
				for j, name := range param.Names {
					if !s.AreAssignmentCompatible(p.Args[offset+j], name) {
						return fmt.Errorf(
							"[Semantic Error at '%s']\n\t mismatched parameters: argument at position '%d' in "+
								"procedure call (%s) does not match parameter at position '%d' in procedure '%s'",
							p.Args[offset+j].Pos(), offset+j, p, offset+j, pHead)
					}
				}

				offset += len(param.Names)
			default:
				if !s.AreAssignmentCompatible(p.Args[offset], param) {
					return fmt.Errorf(
						"[Semantic Error at '%s']\n\t mismatched parameters: argument at position '%d' in "+
							"procedure call (%s) does not match parameter at position '%d' in procedure '%s'",
						p.Args[offset].Pos(), offset+1, p, offset+1, pHead)
				}

				offset++
			}
		}
	}

	return nil
}

func (s *Visitor) VisitNil(n *ast.NilValue) error {
	n.EType = structured.NewPointer()
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
	var err error

	if err = w.E.Accept(s); err != nil {
		return err
	}

	if w.TotalWidth != nil {
		if err = w.TotalWidth.Accept(s); err != nil {
			return err
		}
	}

	if w.FracDigits != nil {
		if err = w.FracDigits.Accept(s); err != nil {
			return err
		}
	}

	return nil
}

func (s *Visitor) VisitSetConstructor(st *ast.SetConstructor) error {
	for _, m := range st.Members {
		if err := m.Accept(s); err != nil {
			return err
		}

		if _, ok := m.Type().(types.Ordinal); !ok {
			return fmt.Errorf(
				"[Semantic Error at '%s']\n\t set member '%s' must be of type, integer, char, Boolean, enum or "+
					"subrange type, currently it is a '%s' type", m.Pos(), m, m.Type())
		}
	}

	if len(st.Members) == 0 {
		st.EType = &types.Set{TokenKind: token.Set}
	} else {
		st.EType = &types.Set{TokenKind: token.Set, BaseType: st.Members[0].Type().(types.Ordinal)}
	}

	return nil
}

func (s *Visitor) VisitValueParam(v *ast.ValueParam) error {
	for _, param := range v.Names {
		if s.symbolTable.DeclaredLocally(param.Name) {
			return fmt.Errorf("[Semantic Error at '%s']\n\t symbol '%s' already defined as type '%s'",
				param.Pos(), param.Name, v.Typ)
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
			return fmt.Errorf("[Semantic Error at '%s']\n\t symbol '%s' already defined as type '%s'",
				param.Pos(), param.Name, v.Typ)
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

func (s *Visitor) VisitVarDecl(decl *ast.VarDeclaration) error {
	for _, varDecl := range decl.Decls {
		for _, id := range varDecl.Names {
			if !s.symbolTable.DeclaredLocally(id.Name) {
				s.symbolTable.EnterSymbol(id.Name, NewVariable(id.Name, VARIABLE, varDecl.Type))
			}
		}
	}

	return nil
}

func (s *Visitor) VisitConstDef(constDef *ast.ConstDefinition) error {
	for _, constDef := range constDef.Consts {
		if !s.symbolTable.DeclaredLocally(constDef.Name.Name) {
			s.symbolTable.EnterSymbol(
				constDef.Name.Name, NewConst(constDef.Name.Name, CONST, constDef.Value.Type(), constDef.Value))
		}
	}

	return nil
}

func (s *Visitor) VisitLabelDef(label *ast.LabelDefinition) error {
	for _, lbl := range label.Labels {
		if !s.symbolTable.DeclaredLocally(lbl.Value) {
			s.symbolTable.EnterSymbol(lbl.Value, NewLabel(lbl.Value, LABEL, base.NewInteger()))
		}
	}

	return nil
}

func (s *Visitor) VisitTypeDef(types *ast.TypeDefinition) error {
	for _, typeDef := range types.Types {
		if !s.symbolTable.DeclaredLocally(typeDef.Name.Name) {
			s.symbolTable.EnterSymbol(typeDef.Name.Name, NewTypeDef(typeDef.Name.Name, TYPE, typeDef.TypeDenoter))
		}
	}

	return nil
}
