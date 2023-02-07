package semantics

import (
	"testing"

	"github.com/anthonyabeo/pasc/pkg/parser"
)

func TestStaticTypeCheckAssignmentStatement(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, sum : integer;

	begin
		a := 1;
		b := 2;

		writeln('Hello, world!')
	end.
`
	lex := parser.NewLexer(input)
	pars, err := parser.NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil || prog == nil {
		t.Error(err)
	}

	semAnal := &SemanticAnalyzer{Ast: prog, SymbolTable: pars.SymbolTable()}
	if err := semAnal.Run(); err != nil {
		t.Error(err)
	}
}

func TestStaticCheckIfStatement(t *testing.T) {
	input := `
	program HelloWorld;
	var
		n, m, result : integer;

	begin
		if (n > m) then
			result := n
		else
			result := m;

		writeln('Hello, world!')
	end.
`
	lex := parser.NewLexer(input)
	pars, err := parser.NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil || prog == nil {
		t.Error(err)
	}

	semAnal := &SemanticAnalyzer{Ast: prog, SymbolTable: pars.SymbolTable()}
	if err := semAnal.Run(); err != nil {
		t.Error(err)
	}
}

func TestStaticCheckMaxProgram(t *testing.T) {
	input := `
	program MaxProgram;
	var 
		a, b, sum : integer;

	function max(n, m integer): integer;
	var result: integer;

	begin
		if (n > m) then
			result := n
		else
			result := m;

		max := result
	end;

	begin
		a := 100;
		b := 200;
		result := max(a, b);

		writeln(max)
	end.
	`

	lex := parser.NewLexer(input)
	pars, err := parser.NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil || prog == nil {
		t.Error(err)
	}

	semAnal := &SemanticAnalyzer{Ast: prog, SymbolTable: pars.SymbolTable()}
	if err := semAnal.Run(); err != nil {
		t.Error(err)
	}
}
