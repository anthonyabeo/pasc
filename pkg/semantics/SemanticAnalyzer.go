package semantics

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/base"
)

// SemanticAnalyzer ...
type SemanticAnalyzer struct {
	Ast         *ast.ProgramAST
	SymbolTable symbols.Scope
}

func (s *SemanticAnalyzer) computeStaticExprType(node ast.Node) error {
	var err error

	switch node := node.(type) {
	case *ast.AssignStatement:
		err = s.computeStaticExprType(node.Variable)
		if err != nil {
			return err
		}

		err = s.computeStaticExprType(node.Value)
		if err != nil {
			return err
		}

	case *ast.UIntegerLiteral:
		node.EvalType = s.SymbolTable.Resolve("integer")

	case *ast.URealLiteral:
		node.EvalType = s.SymbolTable.Resolve("real")

	case *ast.Identifier:
		node.EvalType = node.Scope.Resolve(node.Name).GetType()

	case *ast.IfStatement:
		err = s.computeStaticExprType(node.BoolExpr)
		if err != nil {
			return err
		}

		err = s.computeStaticExprType(node.TruePath)
		if err != nil {
			return err
		}

		err = s.computeStaticExprType(node.ElsePath)
		if err != nil {
			return err
		}

	case *ast.BinaryExpression:
		if s.isArithOp(node.Operator) {
			err = s.computeStaticExprType(node.Left)
			if err != nil {
				return err
			}

			err = s.computeStaticExprType(node.Right)
			if err != nil {
				return err
			}

			node.EvalType, err = s.arithmeticTypeComputation(node)
			if err != nil {
				return err
			}
		}

		if s.isRelationalOp(node.Operator) {
			// TODO check that the left and right operands are orderable. If they are not, return a error
			node.EvalType = s.SymbolTable.Resolve("Boolean").(types.Type)
		}

	case *ast.UnaryExpression:
		if err := s.computeStaticExprType(node.Operand); err != nil {
			return err
		}

		operandType := node.Operand.Attr("type").(types.Type).GetName()

		if operandType != "integer" && operandType != "real" {
			return fmt.Errorf(
				"operands of type %v not supported for '/' operation. They must of type integer or real", operandType)
		}

		node.EvalType = node.Operand.Attr("type").(types.Type)

	case *ast.FuncDesignator:
		funcSymbol := node.Scope.Resolve(node.Name.Name)
		if funcSymbol == nil {
			return fmt.Errorf("function %s not defined", node.Name.Name)
		}

		node.EvalType = funcSymbol.GetType()

	case *ast.FuncDeclaration:
		for _, call := range node.Block.Callables {
			err = s.computeStaticExprType(call)
			if err != nil {
				return err
			}

			err := s.staticTypeCheck(call)
			if err != nil {
				return err
			}
		}

		for _, stmt := range node.Block.Stats {
			err = s.computeStaticExprType(stmt)
			if err != nil {
				return err
			}

			err = s.staticTypeCheck(stmt)
			if err != nil {
				return err
			}
		}
	}

	return nil
}

func (s *SemanticAnalyzer) arithmeticTypeComputation(n *ast.BinaryExpression) (types.Type, error) {
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
	}

	return typ, nil
}

func (s *SemanticAnalyzer) staticTypeCheck(node ast.Node) error {
	var err error

	switch node := node.(type) {
	case *ast.AssignStatement:
		err = s.tcAssignStmt(node)
		if err != nil {
			return err
		}

	case *ast.IfStatement:
		err = s.tcIfStatement(node)
		if err != nil {
			return err
		}

	}

	return nil
}

func (s *SemanticAnalyzer) tcAssignStmt(node *ast.AssignStatement) error {
	if !areAssignmentCompatible(node.Variable.Attr("type").(types.Type), node.Value.Attr("type").(types.Type)) {
		return fmt.Errorf(
			"TypeError: cannot assign value of type %s to variable of type %s",
			node.Value.Attr("type"), node.Variable.Attr("type"),
		)
	}

	return nil
}

func (s *SemanticAnalyzer) tcIfStatement(node *ast.IfStatement) error {
	if node.BoolExpr.Attr("type").(types.Type).GetName() != "Boolean" {
		return fmt.Errorf("if-statement condition does not evaluate to boolean type")
	}
	return nil
}

func (s *SemanticAnalyzer) isMultiplyOp(t token.Token) bool {
	return t.Kind == token.Star ||
		t.Kind == token.FwdSlash ||
		t.Kind == token.Div ||
		t.Kind == token.Mod ||
		t.Kind == token.And
}

func (s *SemanticAnalyzer) isAddingOp(t token.Token) bool {
	return t.Kind == token.Plus ||
		t.Kind == token.Minus ||
		t.Kind == token.Or
}

func (s *SemanticAnalyzer) isRelationalOp(t token.Token) bool {
	return t.Kind == token.Equal ||
		t.Kind == token.LessThanGreaterThan ||
		t.Kind == token.LessThan ||
		t.Kind == token.GreaterThan ||
		t.Kind == token.LessThanOrEqual ||
		t.Kind == token.GreaterThanOrEqual ||
		t.Kind == token.In

}

func (s *SemanticAnalyzer) isArithOp(t token.Token) bool {
	return s.isAddingOp(t) || s.isMultiplyOp(t)
}

// Run ...
func (s *SemanticAnalyzer) Run() error {
	for _, call := range s.Ast.Block.Callables {
		if err := s.computeStaticExprType(call); err != nil {
			return err
		}

		if err := s.staticTypeCheck(call); err != nil {
			return err
		}
	}

	for _, stmt := range s.Ast.Block.Stats {
		if err := s.computeStaticExprType(stmt); err != nil {
			return err
		}

		if err := s.staticTypeCheck(stmt); err != nil {
			return err
		}
	}

	return nil
}

// TODO complete implementation
// a) T1 and T2 are the same type.
// b) T1 is a subrange of T2, or T2 is a subrange of T1, or both T1 and T2 are subranges of the same host-type.
// c) T1 and T2 are set-types of compatible base-types, and either both T1 and T2 are designated packed or neither T1 nor T2 is designated packed.
// d) T1 and T2 are string-types with the same number of components.
func areCompatibleTypes(a, b types.Type) bool {
	if equals(a, b) {
		return true
	}

	return false
}

// TODO complete implementation
// a) T1 and T2 are the same type, and that type is permissible as the component-type of a file-type (see 6.4.3.5).
// b) T1 is the real-type and T2 is the integer-type.
// c) T1 and T2 are compatible ordinal-types, and the value of type T2 is in the closed interval specified by the type T1.
// d) T1 and T2 are compatible set-types, and all the members of the value of type T2 are in the closed interval speci ed by the base-type of T1.
// e) T1 and T2 are compatible string-types.
func areAssignmentCompatible(src, dest types.Type) bool {
	if dest.GetName() == "real" && src.GetName() == "integer" {
		return true
	}

	if areCompatibleTypes(src, dest) {
		return true
	}

	return false
}

func equals(a, b types.Type) bool {
	return a.GetName() == b.GetName()
}
