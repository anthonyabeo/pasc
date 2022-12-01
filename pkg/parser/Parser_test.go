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

	procedureID := procStat.TokenLiteral()
	if procedureID != "writeln" {
		t.Errorf("expected procedure name, writeln, found %v", procedureID)
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

func TestParsingProgramWithAssignmentStatements(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, sum : integer;

	begin
		a := 1;
		b := 2;

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

	if len(prog.Stats) != 3 {
		t.Errorf("expected 3 statement in program; found %v", len(prog.Stats))
	}

	if len(prog.Vars) != 1 {
		t.Errorf("expected 1 var declaration; found %v", len(prog.Vars))
	}

	// first statement
	assignStmt, ok := prog.Stats[0].(*ast.AssignStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.AssignStatement; found %v", assignStmt)
	}

	if assignStmt.Variable.Token.Kind != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.GetTokenName(token.Identifier), token.GetTokenName(assignStmt.Variable.Token.Kind))
	}

	intLit, ok := assignStmt.Value.(*ast.IntegerLiteral)
	if !ok || intLit.Token.Kind != token.IntLiteral {
		t.Errorf("expected value of assignment type to be integer literal")
	}

	// second statement
	assignStmt, ok = prog.Stats[1].(*ast.AssignStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.AssignStatement; found %v", assignStmt)
	}

	if assignStmt.Variable.Token.Kind != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.GetTokenName(token.Identifier), token.GetTokenName(assignStmt.Variable.Token.Kind))
	}

	intLit, ok = assignStmt.Value.(*ast.IntegerLiteral)
	if !ok || intLit.Token.Kind != token.IntLiteral {
		t.Errorf("expected value of assignment type to be integer literal")
	}

	if intLit.Value != "2" {
		t.Errorf("expected value to be 2, got %v", intLit.Value)
	}

	// third statement
	procStat, ok := prog.Stats[2].(*ast.ProcedureStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.ProcedureStatement; found %v", procStat)
	}

	procedureID := procStat.TokenLiteral()
	if procedureID != "writeln" {
		t.Errorf("expected procedure name, writeln, found %v", procedureID)
	}
}

func TestParseBasicArithmeticOperation(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, sum : integer;

	begin
		a := 1;
		b := 2;
		sum := a + b;
		a := -5;

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

	if len(prog.Stats) != 5 {
		t.Errorf("expected 5 statement in program; found %v", len(prog.Stats))
	}

	if len(prog.Vars) != 1 {
		t.Errorf("expected 1 var declaration; found %v", len(prog.Vars))
	}

	stmt, ok := prog.Stats[2].(*ast.AssignStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.AssignStatement; found %v", stmt)
	}

	if stmt.Variable.Token.Kind != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.GetTokenName(token.Identifier), token.GetTokenName(stmt.Variable.Token.Kind))
	}

	expr, ok := stmt.Value.(*ast.BinaryExpression)
	if !ok {
		t.Errorf("expected RHS of to be ast.BinaryExpression, got %v", expr)
	}

	if expr.Operator.Kind != token.Plus {
		t.Errorf("expected operator to be of type +, got %v", expr.Operator.Text)
	}

	lhs, ok := expr.Left.(*ast.Identifier)
	if !ok {
		t.Errorf("expected LHS of binary expression to be of type identifier")
	}

	if lhs.Token.Text != "a" {
		t.Errorf("expected LHS identifier to be 'a', got %v", lhs.Token.Text)
	}

	// Unary Statement
	stmt, ok = prog.Stats[3].(*ast.AssignStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.AssignStatement; found %v", stmt)
	}

	if stmt.Variable.Token.Kind != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.GetTokenName(token.Identifier), token.GetTokenName(stmt.Variable.Token.Kind))
	}

	uexpr, ok := stmt.Value.(*ast.UnaryExpression)
	if !ok {
		t.Errorf("expected RHS of to be ast.UnaryExpression")
	}

	if uexpr.Operator.Kind != token.Minus {
		t.Errorf("expected operator to be of type -, got %v", expr.Operator.Text)
	}

	intLit, ok := uexpr.Operand.(*ast.IntegerLiteral)
	if !ok {
		t.Errorf("expected operand to be integer literal, got %v", intLit)
	}

	if intLit.Value != "5" {
		t.Errorf("expected integer value to be 5, got %v", intLit.Value)
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
