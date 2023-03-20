package semantics

import (
	"github.com/anthonyabeo/pasc/pkg/ast"
)

// SemanticAnalyzer ...
type SemanticAnalyzer struct {
	Ast               *ast.ProgramAST
	ExprEval          *ExprEvalVisitor
	StaticTypeChecker *StaticTypeCheckVisitor
}

// Run ...
func (s *SemanticAnalyzer) Run() {
	for _, call := range s.Ast.Block.Callables {
		Walk(s.ExprEval, call)
		Walk(s.StaticTypeChecker, call)
	}

	for _, stmt := range s.Ast.Block.Stats {
		Walk(s.ExprEval, stmt)
		Walk(s.StaticTypeChecker, stmt)
	}

}
