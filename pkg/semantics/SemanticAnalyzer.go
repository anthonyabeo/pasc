package semantics

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
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

		}

		if s.isRelationalOp(node.Operator) {
			// TODO check that the left and right operands are orderable. If they are not, return a error
			node.EvalType = s.SymbolTable.Resolve("Boolean")
		}

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
	if node.Value.Attr("type") != node.Variable.Attr("type") {
		return fmt.Errorf(
			"TypeError: cannot assign value of type %s to variable of type %s",
			node.Value.Attr("type"), node.Variable.Attr("type"),
		)
	}

	return nil
}

func (s *SemanticAnalyzer) tcIfStatement(node *ast.IfStatement) error {
	if node.BoolExpr.Attr("type") != "Boolean" {
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
