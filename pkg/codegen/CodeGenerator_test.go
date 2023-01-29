package codegen

import (
	"testing"

	"github.com/anthonyabeo/pasc/pkg/parser"
	"github.com/anthonyabeo/pasc/pkg/sematics"
)

func TestGenerateBrilProgramAssignment(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a : integer;

	begin
		a := 1;

		writeln( a )
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

	semAnal := &sematics.SemanticAnalyzer{Ast: prog, SymbolTable: pars.SymbolTable()}
	if err := semAnal.Run(); err != nil {
		t.Error(err)
	}

	cg, err := NewCodeGenerator(prog.Name.Name)
	if err != nil {
		t.Error(err)
	}

	if err := cg.Gen(prog); err != nil {
		t.Error(err)
	}
}

func TestGenerateBrilProgramArithmeticOperation(t *testing.T) {
	input := `
	program Add;
	var
		a, b, result : integer;

	begin
		a := 5;
		b := 15;
		result := a + b;

		writeln( result )
	end.
`
	lex := parser.NewLexer(input)
	pars, err := parser.NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	semAnal := &sematics.SemanticAnalyzer{Ast: prog, SymbolTable: pars.SymbolTable()}
	if err := semAnal.Run(); err != nil {
		t.Error(err)
	}

	cg, err := NewCodeGenerator(prog.Name.Name)
	if err != nil {
		t.Error(err)
	}

	if err := cg.Gen(prog); err != nil {
		t.Error(err)
	}
}

func TestGenerateBrilProgramIfStatement(t *testing.T) {
	input := `
	program HelloWorld;
	var
		n, m, result : integer;

	begin
		n := 2;
		m := 15;

		if (n > m) then
			result := n
		else
			result := m;

		writeln( result )
	end.
`
	lex := parser.NewLexer(input)
	pars, err := parser.NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	semAnal := &sematics.SemanticAnalyzer{Ast: prog, SymbolTable: pars.SymbolTable()}
	if err := semAnal.Run(); err != nil {
		t.Error(err)
	}

	cg, err := NewCodeGenerator(prog.Name.Name)
	if err != nil {
		t.Error(err)
	}

	if err := cg.Gen(prog); err != nil {
		t.Error(err)
	}
}

func TestGenerateBrilProgramWithFunctionCall(t *testing.T) {
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
		a := 500;
		b := 200;
		result := max(a, b);

		writeln( result )
	end.
	`
	lex := parser.NewLexer(input)
	pars, err := parser.NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	semAnal := &sematics.SemanticAnalyzer{Ast: prog, SymbolTable: pars.SymbolTable()}
	if err := semAnal.Run(); err != nil {
		t.Error(err)
	}

	cg, err := NewCodeGenerator(prog.Name.Name)
	if err != nil {
		t.Error(err)
	}

	if err := cg.Gen(prog); err != nil {
		t.Error(err)
	}
}
