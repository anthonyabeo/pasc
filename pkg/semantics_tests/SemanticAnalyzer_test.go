package symbols

import (
	"testing"

	"github.com/anthonyabeo/pasc/pkg/parser"
	"github.com/anthonyabeo/pasc/pkg/semantics"
)

func TestStaticTypeCheckAssignmentStatement(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, sum : integer;

	begin
		a := 1
	end.
`
	lex := parser.NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := parser.NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil || program == nil {
		t.Error(err)
	}

	//sema := &SemanticAnalyzer{
	//	Ast:               prog,
	//	ExprEval:          &ExprEvalVisitor{SymbolTable: pars.SymbolTable()},
	//	StaticTypeChecker: &StaticTypeCheckVisitor{},
	//}
	//sema.Run()

	sema := semantics.NewSemaVisitor(program, symTable)
	sema.VisitProgram()
}

func TestStaticTypeCheckBasicArithmetic(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, sum : integer;

	begin
		a := 1;
		b := 2;
		sum := a + b
	end.
`
	lex := parser.NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := parser.NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil || program == nil {
		t.Error(err)
	}

	sema := semantics.NewSemaVisitor(program, symTable)
	sema.VisitProgram()
}

func TestTypeCheckWhileStatement(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, sum : integer;

	begin
		sum := 0;
		a := 5;

		while a >= 0 do
		begin
			sum := sum + a;
			a := a - 1
		end;

		writeln(sum)
	end.
`
	lex := parser.NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := parser.NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil || program == nil {
		t.Error(err)
	}

	sema := semantics.NewSemaVisitor(program, symTable)
	sema.VisitProgram()
}

//func TestStaticCheckIfStatement(t *testing.T) {
//	input := `
//	program HelloWorld;
//	var
//		n, m, result : integer;
//
//	begin
//		if (n > m) then
//			result := n
//		else
//			result := m;
//
//		writeln('Hello, world!')
//	end.
//`
//	lex := parser.NewLexer(input)
//	pars, err := parser.NewParser(lex)
//	if err != nil {
//		t.Error(err)
//	}
//
//	prog, err := pars.Program()
//	if err != nil || prog == nil {
//		t.Error(err)
//	}
//
//	sema := &SemanticAnalyzer{
//		Ast:               prog,
//		ExprEval:          &ExprEvalVisitor{SymbolTable: pars.SymbolTable()},
//		StaticTypeChecker: &StaticTypeCheckVisitor{},
//	}
//	sema.Run()
//}
//
//func TestStaticCheckMaxProgram(t *testing.T) {
//	input := `
//	program MaxProgram;
//	var
//		a, b, sum : integer;
//
//	function max(n, m : integer): integer;
//	var result: integer;
//
//	begin
//		if (n > m) then
//			result := n
//		else
//			result := m;
//
//		max := result
//	end;
//
//	begin
//		a := 100;
//		b := 200;
//		result := max(a, b);
//
//		writeln(result)
//	end.
//	`
//
//	lex := parser.NewLexer(input)
//	pars, err := parser.NewParser(lex)
//	if err != nil {
//		t.Error(err)
//	}
//
//	prog, err := pars.Program()
//	if err != nil || prog == nil {
//		t.Error(err)
//	}
//
//	sema := &SemanticAnalyzer{
//		Ast:               prog,
//		ExprEval:          &ExprEvalVisitor{SymbolTable: pars.SymbolTable()},
//		StaticTypeChecker: &StaticTypeCheckVisitor{},
//	}
//	sema.Run()
//}
//
//func TestStaticTypeCheckArithmeticOperation(t *testing.T) {
//	input := `
//	program HelloWorld;
//	var
//		a, b, sum : integer;
//
//	begin
//		a := 1;
//		b := 2;
//
//		sum := a + b;
//
//		writeln( sum )
//	end.
//`
//	lex := parser.NewLexer(input)
//	pars, err := parser.NewParser(lex)
//	if err != nil {
//		t.Error(err)
//	}
//
//	prog, err := pars.Program()
//	if err != nil || prog == nil {
//		t.Error(err)
//	}
//
//	sema := &SemanticAnalyzer{
//		Ast:               prog,
//		ExprEval:          &ExprEvalVisitor{SymbolTable: pars.SymbolTable()},
//		StaticTypeChecker: &StaticTypeCheckVisitor{},
//	}
//	sema.Run()
//}
//

//
//func TestTypeCheckForStatement(t *testing.T) {
//	input := `
//	program HelloWorld;
//
//	type
//		arr = array [integer] of integer;
//
//	var
//		i, max : integer;
//
//	begin
//		for i := 2 to 63 do
//			if arr[i] > max then
//				max := arr[i];
//
//		writeln(max)
//	end.
//`
//
//	lex := parser.NewLexer(input)
//	pars, err := parser.NewParser(lex)
//	if err != nil {
//		t.Error(err)
//	}
//
//	prog, err := pars.Program()
//	if err != nil || prog == nil {
//		t.Error(err)
//	}
//
//	sema := &SemanticAnalyzer{
//		Ast:               prog,
//		ExprEval:          &ExprEvalVisitor{SymbolTable: pars.SymbolTable()},
//		StaticTypeChecker: &StaticTypeCheckVisitor{},
//	}
//	sema.Run()
//}
