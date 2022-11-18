package parser

import (
	"testing"

	"github.com/anthonyabeo/pasc/pkg/ast"
)

func TestParseBasicProgram(t *testing.T) {
	input := `
	program HelloWorld;
	begin
		writeln('Hello, World!');
	end.
`

	lex := NewLexer(input)
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if prog == nil {
		t.Error("AST not created")
	}

	if len(prog.Stats) != 1 {
		t.Errorf("expected 1 statement in program; found %v", len(prog.Stats))
	}

	procStat, ok := prog.Stats[0].(*ast.ProcedureStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.ProcedureStatement; found %v", procStat)
	}

	programName := procStat.TokenLiteral()
	if programName != "writeln" {
		t.Errorf("expected procedure name, HelloWorld, found %v", programName)
	}
}
