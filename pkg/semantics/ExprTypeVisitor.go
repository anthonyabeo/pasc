package semantics

import (
	"fmt"
	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
	"github.com/anthonyabeo/pasc/pkg/types/structured"
)

// ExprEvalVisitor ...
type ExprEvalVisitor struct {
	SymbolTable symbols.Scope
}

// Visit ...
func (v *ExprEvalVisitor) Visit(node ast.Node) {
	var err error

	switch node := node.(type) {
	case *ast.ProcedureStmt:
		for _, param := range node.ParamList {
			v.Visit(param)
		}
	case *ast.Writeln:
		for _, param := range node.ParamList {
			v.Visit(param)
		}
	case *ast.WriteParameter:
		v.Visit(node.E)
		if node.TotalWidth != nil {
			v.Visit(node.TotalWidth)
		}

		if node.FracDigits != nil {
			v.Visit(node.FracDigits)
		}
	case *ast.AssignStatement:
		v.Visit(node.Variable)
		v.Visit(node.Value)
	case *ast.IfStatement:
		v.Visit(node.BoolExpr)
		v.Visit(node.TruePath)
		if node.ElsePath != nil {
			v.Visit(node.ElsePath)
		}
	case *ast.FuncDeclaration:
		for _, call := range node.Block.Callables {
			v.Visit(call)
		}

		for _, stmt := range node.Block.Stats {
			v.Visit(stmt)
		}
	case *ast.FuncDesignator:
		funcSymbol := node.Scope.Resolve(node.Name.Name)
		if funcSymbol == nil {
			panic(fmt.Sprintf("function %s not defined", node.Name.Name))
		}

		node.EvalType = funcSymbol.GetType()
	case *ast.UIntegerLiteral:
		node.EvalType = v.SymbolTable.Resolve("integer")
	case *ast.Identifier:
		sym := node.Scope.Resolve(node.Name)
		if sym == nil {
			panic(fmt.Sprintf("symbol %v is undefined", node.Name))
		}
		node.EvalType = sym.GetType()
	case *ast.CharString:
		if len(node.Value) > 1 {
			node.EvalType = &base.String{Name: "string"}
		}

		node.EvalType = &base.Char{Name: "char"}
	case *ast.URealLiteral:
		node.EvalType = v.SymbolTable.Resolve("real")
	case *ast.BinaryExpression:
		v.Visit(node.Left)
		v.Visit(node.Right)

		if v.isArithOp(node.Operator) {
			node.EvalType, err = v.arithmeticTypeComputation(node)
			if err != nil {
				panic(err)
			}
		}

		if v.isRelationalOp(node.Operator) {
			node.EvalType, err = v.relExprTypeComputation(node)
			if err != nil {
				panic(err)
			}
		}
	case *ast.UnaryExpression:
		v.Visit(node.Operand)
		node.EvalType = node.Operand.Attr("type").(types.Type)
	case *ast.WhileStatement:
		v.Visit(node.BoolExpr)
		v.Visit(node.Body)
	case *ast.CompoundStatement:
		for _, stmt := range node.Statements {
			v.Visit(stmt)
		}
	case *ast.ForStatement:
		v.Visit(node.CtrlID)
		v.Visit(node.InitValue)
		v.Visit(node.FinalValue)
		v.Visit(node.Body)
	case *ast.IndexedVariable:
		v.Visit(node.ArrayVar)
		for _, idxExpr := range node.IndexExpr {
			v.Visit(idxExpr)
		}

		t := node.ArrayVar.Attr("type").(types.Type)
		for i := 0; i < len(node.IndexExpr); i++ {
			arr, ok := t.(*structured.Array)
			if !ok {
				panic(fmt.Sprintf("expected array type, got %T instead", arr))
			}

			t = arr.ComponentType
		}

		node.EvalType = t
	case *ast.ReturnStatement:
		v.Visit(node.Expr)
	case *ast.RepeatStatement:
		for _, stmt := range node.StmtSeq {
			v.Visit(stmt)
		}
		v.Visit(node.BoolExpr)
	case *ast.GotoStatement:
	case *ast.CaseStatement:
	default:
		panic(fmt.Sprintf("Visit: unexpected expression type %T", node))
	}
}

func (v *ExprEvalVisitor) relExprTypeComputation(n *ast.BinaryExpression) (types.Type, error) {
	// TODO complete implementation

	lhsType := n.Left.Attr("type").(types.Type)
	rhsType := n.Right.Attr("type").(types.Type)

	operandsSimpleType := isSimpleType(lhsType) && isSimpleType(rhsType)
	operandsStringType := lhsType.GetName() == "string" && rhsType.GetName() == "string"
	operandsPointerType := lhsType.GetName() == "pointer" && rhsType.GetName() == "pointer"

	switch n.Operator.Kind {
	case token.Equal, token.LessThanGreaterThan:
		if !operandsSimpleType && !operandsStringType || operandsPointerType {
			return nil, fmt.Errorf(
				"invalid operand types, %v and %v", lhsType.GetName(), rhsType.GetName())
		}

	case token.LessThan, token.GreaterThan:
		if !operandsSimpleType && !operandsStringType {
			return nil, fmt.Errorf(
				"invalid operand types, %v and %v", lhsType.GetName(), rhsType.GetName())
		}

	case token.LessThanOrEqual, token.GreaterThanOrEqual:
		if !operandsSimpleType && !operandsStringType {
			return nil, fmt.Errorf(
				"invalid operand types, %v and %v", lhsType.GetName(), rhsType.GetName())
		}

	case token.In:

	}

	return &base.Boolean{Name: "Boolean"}, nil
}

func (v *ExprEvalVisitor) arithmeticTypeComputation(n *ast.BinaryExpression) (types.Type, error) {
	var typ types.Type

	lhsType := n.Left.Attr("type").(types.Type).GetName()
	rhsType := n.Right.Attr("type").(types.Type).GetName()

	switch n.Operator.Kind {
	case token.Plus, token.Minus, token.Star:
		if (lhsType != "integer" && lhsType != "real") ||
			(rhsType != "integer" && rhsType != "real") {

			return nil, fmt.Errorf(
				"operands of type %v and %v not supported for '+', '-' and '*' operations. They must of type integer or real", lhsType, rhsType)
		} else if lhsType == "integer" && rhsType == "integer" {
			typ = &base.Integer{Name: "integer"}
		} else {
			typ = &base.Real{Name: "real"}
		}
	case token.FwdSlash:
		if lhsType != "integer" && lhsType != "real" &&
			rhsType != "integer" && rhsType != "real" {

			return nil, fmt.Errorf(
				"operands of type %v and %v not supported for '/' operation. They must of type integer or real", lhsType, rhsType)
		}

		typ = &base.Real{Name: "real"}
	case token.Div, token.Mod:
		if lhsType != "integer" && rhsType != "integer" {
			return nil, fmt.Errorf(
				"operands of type %v and %v not supported for 'mod' operation. They must of type integer type", lhsType, rhsType)
		}

		typ = &base.Integer{Name: "integer"}
	case token.Or, token.And:
		typ = &base.Integer{Name: "integer"}
	}

	return typ, nil
}

func (v *ExprEvalVisitor) isMultiplyOp(t token.Token) bool {
	return t.Kind == token.Star ||
		t.Kind == token.FwdSlash ||
		t.Kind == token.Div ||
		t.Kind == token.Mod ||
		t.Kind == token.And
}

func (v *ExprEvalVisitor) isAddingOp(t token.Token) bool {
	return t.Kind == token.Plus ||
		t.Kind == token.Minus ||
		t.Kind == token.Or
}

func (v *ExprEvalVisitor) isRelationalOp(t token.Token) bool {
	return t.Kind == token.Equal ||
		t.Kind == token.LessThanGreaterThan ||
		t.Kind == token.LessThan ||
		t.Kind == token.GreaterThan ||
		t.Kind == token.LessThanOrEqual ||
		t.Kind == token.GreaterThanOrEqual ||
		t.Kind == token.In

}

func (v *ExprEvalVisitor) isArithOp(t token.Token) bool {
	return v.isAddingOp(t) || v.isMultiplyOp(t)
}
