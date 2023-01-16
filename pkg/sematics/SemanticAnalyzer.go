package sematics

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// SemanticAnalyzer ...
type SemanticAnalyzer struct {
	ast         *ast.ProgramAST
	SymbolTable symbols.Scope
}

// ComputeStaticExpressionTypes ...
func (s *SemanticAnalyzer) ComputeStaticExpressionTypes(stmt ast.Statement) error {
	switch stmt.TokenKind() {
	case token.Initialize:
		assignStmt := stmt.(*ast.AssignStatement)
		if err := s.computeStaticExpressionTypes(assignStmt.Variable); err != nil {
			return err
		}
		if err := s.computeStaticExpressionTypes(assignStmt.Value); err != nil {
			return err
		}
		return nil
	case token.If:
		ifStmt := stmt.(*ast.IfStatement)
		if err := s.computeStaticExpressionTypes(ifStmt.BoolExpr); err != nil {
			return err
		}
		if err := s.ComputeStaticExpressionTypes(ifStmt.TruePath); err != nil {
			return err
		}
		if err := s.ComputeStaticExpressionTypes(ifStmt.ElsePath); err != nil {
			return err
		}
		return nil
	case token.Procedure:
		return nil
	default:
		return nil
	}
}

func (s *SemanticAnalyzer) computeStaticExpressionTypes(expr ast.Expression) error {
	switch expr.TokenKind() {
	case token.UIntLiteral:
		uintLit := expr.(*ast.UIntegerLiteral)
		uintLit.EvalType = s.SymbolTable.Resolve("integer")
		return nil
	case token.Identifier:
		id := expr.(*ast.Identifier)
		id.EvalType = id.Scope.Resolve(id.Name).GetType()
		return nil
	case token.LessThan, token.GreaterThan, token.GreaterThanOrEqual, token.LessThanOrEqual, token.LessThanGreaterThan:
		relExpr := expr.(*ast.BinaryExpression)
		// TODO check that the left and right operands are orderable.
		// If they are not, return a error
		relExpr.EvalType = s.SymbolTable.Resolve("Boolean")
		return nil
	default:
		return nil
	}
}

// Run ...
func (s *SemanticAnalyzer) Run() error {
	for _, stmt := range s.ast.Block.Stats {
		if err := s.ComputeStaticExpressionTypes(stmt); err != nil {
			return err
		}

		if err := s.StaticTypeCheck(stmt); err != nil {
			return err
		}
	}

	return nil
}

// StaticTypeCheck ...
func (s *SemanticAnalyzer) StaticTypeCheck(stmt ast.Statement) error {
	switch stmt.TokenKind() {
	case token.Initialize:
		assignStmt := stmt.(*ast.AssignStatement)
		if err := s.assignment(assignStmt); err != nil {
			return err
		}
		return nil
	case token.If:
		ifStmt := stmt.(*ast.IfStatement)
		if err := s.ifStatement(ifStmt); err != nil {
			return err
		}
		return nil
	default:
		return nil
	}
}

func (s *SemanticAnalyzer) assignment(node *ast.AssignStatement) error {
	if node.Value.Attr("type") != node.Variable.Attr("type") {
		return fmt.Errorf(
			"TypeError: cannot assign value of type %s to variable of type %s",
			node.Value.Attr("type"), node.Variable.Attr("type"),
		)
	}

	return nil
}

func (s *SemanticAnalyzer) ifStatement(node *ast.IfStatement) error {
	// TODO implement static type checking
	return nil
}
