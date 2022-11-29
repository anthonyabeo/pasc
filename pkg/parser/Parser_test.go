package parser

import (
	"testing"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/dtype"
	"github.com/anthonyabeo/pasc/pkg/token"
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

func TestParseProgramWithVarDeclarations(t *testing.T) {
	input := `
	program HelloWorld;
	var 
		a, b, sum : integer;

	begin
		writeln('Hello, world!');
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

	if len(prog.Vars) != 1 {
		t.Errorf("expected 1 var declaration; found %v", len(prog.Vars))
	}

	varDecl := prog.Vars[0]
	if varDecl.Token.Kind != token.Var {
		t.Errorf("expected token to be %v; got %v",
			token.GetTokenName(token.Var), token.GetTokenName(varDecl.Token.Kind))
	}

	if len(varDecl.Names) != 3 {
		t.Errorf("expected 3 variables; got %v", len(varDecl.Names))
	}

	intType, ok := varDecl.Type.(*dtype.Integer)
	if !ok {
		t.Errorf("expected variables type to be an integer, instead it is of type, %v", intType)
	}

	if intType.Token.Kind != token.Integer {
		t.Errorf("expected token type to be %v, got %v",
			token.GetTokenName(token.Integer), token.GetTokenName(intType.Token.Kind))
	}
}

// func TestFibonacciProgram(t *testing.T) {
// 	input := `
// 	program MaxProgram;

// 	function max(n, m integer): integer;
// 	var result: integer;

// 	begin
// 		if (n > m) then
// 			result := n;
// 		else
// 			result := m;

// 		max := result
// 	end;

// 	begin
// 		a := 100;
// 		b := 200;
// 		ret := max(a, b);

// 		writeln(ret);
// 	end.
// 	`
// }
