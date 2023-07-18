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
	if err := sema.VisitProgram(); err != nil {
		t.Error(err)
	}
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
	if err := sema.VisitProgram(); err != nil {
		t.Error(err)
	}
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
	if err := sema.VisitProgram(); err != nil {
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
	if err := sema.VisitProgram(); err != nil {
		t.Error(err)
	}
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
	if err := sema.VisitProgram(); err != nil {
		t.Error(err)
	}
}

func TestStaticTypeCheckExpressions(t *testing.T) {
	input := `
	program HelloWorld;
	
	type
		hue = set of integer;
		string = set of char;
		operator = (plus, minus, times, divide);

	var
		a, b, c, i, j, k, red, green : integer;
		x, y, z : real;
		p, q, r : Boolean;
		h1, h2, h3: hue;
		s : string;
		op    : operator;

	function 
		sin(n: integer): real;
		begin
			sin := 0.01
		end;

	begin
		a := 15;
		x := (x + y + z);
		z := sin(a + b);
		h1 := [1, 2, 3, 4, 5];
		h2 := [1, 5, 10..19, 23];
		h3 := [];
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

		op := times;

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
	if err := sema.VisitProgram(); err != nil {
		t.Error(err)
	}
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
	if err := sema.VisitProgram(); err != nil {
		t.Error(err)
	}
}

func TestTypeCheckWhileStatementWithInvalidCondition(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, sum : integer;

	begin
		sum := 0;
		a := 5;

		while a + 0 do
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
	err = sema.VisitProgram()
	if err == nil {
		t.Error("should return an error")
	}

	if err.Error() != "while-statement condition must evaluate to a Boolean, 'a + 0' does not" {
		t.Error("invalid error message")
	}
}

func TestTypeCheckForStatementWithInvalidCtrlID(t *testing.T) {
	input := `
	program HelloWorld;

	var
		i, sum : integer;

	begin
		for i := 1.2 to 63 do
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
	err = sema.VisitProgram()
	if err == nil {
		t.Error("should return an error")
	}

	errMsg := "control variable 'i' (of type 'integer'), is not compatible with initial value of type 'real'"
	if err.Error() != errMsg {
		t.Errorf("expected error message \n\t'%s', got \n\t'%s' instead", errMsg, err.Error())
	}
}

func TestStaticCheckFuncDeclaration(t *testing.T) {
	input := `
	program MaxProgram;
	var
		a, b, sum : integer;

	function max(n, m : integer): real;
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
	err = sema.VisitProgram()
	if err == nil {
		t.Error("should return an error")
	}

	errMsg := "declared return type of max is real, does not match return value type integer"
	if err.Error() != errMsg {
		t.Errorf("expected error message \n\t'%s', got \n\t'%s' instead", errMsg, err.Error())
	}
}

func TestTypeCheckWithStatement(t *testing.T) {
	input := `
		program Record;

		var
			date : record
			   year : integer;
			   month : integer;
			   day : integer
			end;
		
		begin
			date.day := 7;
			date.month := 12;
			date.year := 1992;

			with date do
				if month = 12 then
					begin 
						month := 1; 
						year := year + 1
					end
				else 
					month := month+1;

			writeln('%d/%d/%d', date.day, date.month, date.year)
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
	if err := sema.VisitProgram(); err != nil {
		t.Error(err)
	}
}

func TestFunctionHeadingParameterWithInvalidArgument(t *testing.T) {
	input := `
	program HelloWorld;
	const
		eps = 1e-10;

	var
		midpoint : real;
		foo : integer;

	function bar(x : real) : real;
	begin
		bar := 3.143 + x
	end;

	procedure 
		bisect (function f(x : real) : real;
						a, b         : real;
                var     result       : real);

	begin
		midpoint := f(99.9);
		writeln(midpoint)
	end;

	begin
		bisect(bar, 0.12, 4, 32);
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
	err = sema.VisitProgram()
	if err == nil {
		t.Error("should return an error")
	}

	errMsg := "argument '32' used in procedure call 'bisect(bar, 0.12, 4, 32)' must be a variable"
	if err.Error() != errMsg {
		t.Errorf("expected error message \n\t'%s', got \n\t'%s' instead", errMsg, err.Error())
	}
}

func TestFunctionHeadingParameter(t *testing.T) {
	input := `
	program HelloWorld;
	const
		eps = 1e-10;

	var
		midpoint, slang : real;
		foo : integer;

	function bar(x : real) : real;
	begin
		bar := 3.143 + x
	end;

	procedure 
		bisect (function f(x : real)     : real;
						a, b             : real;
                var     result, gh       : real);

	begin
		midpoint := f(99.9);
		writeln(midpoint)
	end;

	begin
		bisect(bar, 0.12, 4, midpoint, slang);
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
	err = sema.VisitProgram()
	if err != nil {
		t.Error(err)
	}
}

func TestTypeCheckNotUnaryExpression(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b : integer;
		p, q, r : Boolean;

	function 
		sin(n: integer): real;
		begin
			sin := 0.01
		end;

	begin
		r := not sin(a + b);

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
	err = sema.VisitProgram()
	if err == nil {
		t.Error("should return an error")
	}

	errMsg := "not-expression, 'not sin(a + b)' operand must evaluate to a Boolean type, it evaluates to 'real'"
	if err.Error() != errMsg {
		t.Errorf("expected error message \n\t'%s', got \n\t'%s' instead", errMsg, err.Error())
	}
}

func TestTypeCheckMinusUnaryExpression(t *testing.T) {
	input := `
	program HelloWorld;
	type
		hue = set of integer;
		string = set of char;

	var
		a, b : integer;
		p, q, r : Boolean;
		h1, h2, h3: hue;

	function 
		sin(n: integer): real;
		begin
			sin := 0.01
		end;

	begin
		h1 := [1, 2, 3, 4, 5];
		h3 := -[1, 5, 10..19, 23];

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
	err = sema.VisitProgram()
	if err == nil {
		t.Error("should return an error")
	}

	errMsg := "operand, '[1, 5, 10..19, 23]' in expression, '-[1, 5, 10..19, 23]', must be real or integer type. it is a 'set of integer' type"
	if err.Error() != errMsg {
		t.Errorf("expected error message \n\t'%s', got \n\t'%s' instead", errMsg, err.Error())
	}
}
