package semantics_tests

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

func TestStaticCheckMaxProgram(t *testing.T) {
	input := `
	program MaxProgram;
	var
		a, b, sum : integer;

	function max(n, m : integer): integer;
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
		sum := max(a, b);

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

func TestStaticTypeCheckExpressions(t *testing.T) {
	input := `
	program HelloWorld;
	
	type
		hue = set of integer;
		string = set of char;

	var
		a, b, c, i, j, k, red, green : integer;
		x, y, z : real;
		p, q, r : Boolean;
		h1, h2: hue;
		s : string;

	function 
		sin(n: integer): real;
		begin
			sin := 0.01
		end;

	begin
		a := 15;
		x := (x + y + z);
		z := sin(x + y);
		h1 := [1, 2, 3, 4, 5];
		h2 := [1, 5, 10..19, 23];
		r := not p;

		x := x * y;
		y := i / (1 - i);
		p := (x <= y) and (y < z);

		q := p or q;
		y := x + y;
		y := -x;
		h1 := h1 + h2;
		c := i * j + 1;

		x := 1.5;
		r := p <= q;
		r := p = q and r;
		r := (i < j) or (j < k);
		r := c in h1;

		writeln('Hello, world!')
	end.
`
	lex := parser.NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := parser.NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	sema := semantics.NewSemaVisitor(program, symTable)
	sema.VisitProgram()
}

func TestTypeCheckForStatement(t *testing.T) {
	input := `
	program HelloWorld;

	var
		i, sum : integer;

	begin
		for i := 2 to 63 do
			sum := sum + 1;

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
