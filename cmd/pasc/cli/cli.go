package cli

import (
	"fmt"
	"os"

	serde "github.com/anthonyabeo/pasc/pkg/codegen/serializer"
	"github.com/anthonyabeo/pasc/pkg/parser"
	"github.com/anthonyabeo/pasc/pkg/semantics"
)

// Run ...
func Run(args []string) error {
	if len(args) < 1 {
		return fmt.Errorf("program file missing")
	}

	input, err := os.ReadFile(args[0])
	if err != nil {
		return err
	}

	lex := parser.NewLexer(string(input))
	pars, err := parser.NewParser(lex)
	if err != nil {
		return err
	}

	prog, err := pars.Program()
	if err != nil {
		return err
	}

	sema := &semantics.SemanticAnalyzer{
		Ast:               prog,
		ExprEval:          &semantics.ExprEvalVisitor{SymbolTable: pars.SymbolTable()},
		StaticTypeChecker: &semantics.StaticTypeCheckVisitor{},
	}
	sema.Run()

	serializer := &serde.ProtoSerializer{Ast: prog}
	err = serializer.Serialize()
	if err != nil {
		return err
	}

	return nil
}
