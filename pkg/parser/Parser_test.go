package parser

import (
	"github.com/anthonyabeo/pasc/pkg/semantics"
	"testing"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types/base"
)

func TestParseBasicProgram(t *testing.T) {
	input := `
	program HelloWorld;
	begin
		writeln('Hello, World!')
	end.
`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 1, 0, 0, 0, 0) {
		return
	}

	args := []ast.Expression{
		&ast.StrLiteral{TokenKind: token.StrLiteral, Value: "Hello, World!"},
	}
	if !testWriteln(t, program.Block.Stats[0], "writeln", args) {
		return
	}
}

func TestParseProgramWithVarDeclarations(t *testing.T) {
	input := `
	program HelloWorld;
	var 
		a, b, sum : integer;

	begin
		writeln('Hello, world!')
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 1, 1, 0, 0, 0) {
		return
	}

	if !testVarDeclaration(
		t, program.Block.VarDeclaration, token.NewToken(token.Var, "var"), 1,
		[][]string{{"a", "b", "sum"}}, []string{"integer"}) {
		return
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

		writeln('Hello, world!')
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 3, 1, 0, 0, 0) {
		return
	}

	// first statement
	value := &ast.UIntegerLiteral{TokenKind: token.UIntLiteral, Value: "1"}
	if !testAssignmentStatement(t, program.Block.Stats[0], "a", value) {
		return
	}

	// second statement
	value = &ast.UIntegerLiteral{TokenKind: token.UIntLiteral, Value: "2"}
	if !testAssignmentStatement(t, program.Block.Stats[1], "b", value) {
		return
	}

	// third statement
	args := []ast.Expression{
		&ast.StrLiteral{TokenKind: token.StrLiteral, Value: "Hello, world!"},
	}

	if !testWriteln(t, program.Block.Stats[2], "writeln", args) {
		return
	}
}

func testWriteln(t *testing.T, stmt ast.Statement, procName string, args []ast.Expression) bool {
	return true
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

		writeln('Hello, world!')
	end.
`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 5, 1, 0, 0, 0) {
		return
	}

	expr := &ast.BinaryExpression{
		Operator: token.NewToken(token.Plus, "+"),
		Left:     &ast.Identifier{TokenKind: token.Identifier, Name: "a"},
		Right:    &ast.Identifier{TokenKind: token.Identifier, Name: "b"},
	}

	if !testAssignmentStatement(t, program.Block.Stats[2], "sum", expr) {
		return
	}

	stmt := program.Block.Stats[2].(*ast.AssignStatement)
	if !testBinaryExpression(t, stmt.Value, expr.Operator, "a", "b") {
		return
	}

	uexpr := &ast.UnaryExpression{
		Operator: token.NewToken(token.Minus, "-"),
		Operand:  &ast.UIntegerLiteral{TokenKind: token.UIntLiteral, Value: "5"},
	}
	if !testAssignmentStatement(t, program.Block.Stats[3], "a", uexpr) {
		return
	}

	stmt = program.Block.Stats[3].(*ast.AssignStatement)
	if !testUnaryExpression(t, stmt.Value, token.NewToken(token.Minus, "-"), "5") {
		return
	}
}

func TestParseProgramWithFunctionDeclaration(t *testing.T) {
	input := `
	program MaxProgram;

	var 
		a, b : integer;

	function 
		foo(n, m :integer): integer;
		var 
			result: integer;

		begin
			foo := 2
		end;

	begin
		a := 100;
		b := 200;

		writeln('Hello, world!')
	end.
	`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "MaxProgram", []string{}, 3, 1, 1, 0, 0) {
		return
	}

	params := []ast.FormalParameter{
		&ast.ValueParam{
			Names: []*ast.Identifier{
				{TokenKind: token.Identifier, Name: "n"},
				{TokenKind: token.Identifier, Name: "m"},
			},
			Type: base.NewInteger(),
		},
	}
	if !testFuncDeclaration(t, program.Block.Callables[0], "foo", "integer", params, 1, 1, 0, 0, 0) {
		return
	}

	funcDecl := program.Block.Callables[0].(*ast.FuncDeclaration)
	if !testReturnStatement(t, funcDecl.Block.Stats[0], "2") {
		return
	}
}

func TestParseProgramWithIfStatement(t *testing.T) {
	input := `
	program MaxProgram;
	var 
		a, b, sum : integer;

	function 
		max(n, m : integer): integer;
		var 
			result: integer;

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

		writeln('Hello, world!')
	end.
	`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "MaxProgram", []string{}, 3, 1, 1, 0, 0) {
		return
	}

	paramList := []ast.FormalParameter{
		&ast.ValueParam{
			Names: []*ast.Identifier{
				{TokenKind: token.Identifier, Name: "n"},
				{TokenKind: token.Identifier, Name: "m"},
			},
			Type: base.NewInteger(),
		},
	}
	if !testFuncDeclaration(t, program.Block.Callables[0], "max", "integer", paramList, 2, 1, 0, 0, 0) {
		return
	}
	funcDecl := program.Block.Callables[0].(*ast.FuncDeclaration)
	if !testIfStatement(t, funcDecl.Block.Stats[0], token.NewToken(token.GreaterThan, ">"), "n", "m", "result := n", "result := m") {
		return
	}

	ifStmt := funcDecl.Block.Stats[0].(*ast.IfStatement)

	value := &ast.Identifier{TokenKind: token.Identifier, Name: "n"}
	if !testAssignmentStatement(t, ifStmt.TruePath, "result", value) {
		return
	}

	value.Name = "m"
	if !testAssignmentStatement(t, ifStmt.ElsePath, "result", value) {
		return
	}
}

func TestParseProgramWithFunctionCall(t *testing.T) {
	input := `
	program MaxProgram;
	var
		a, b, result : integer;
	
	function 
		max(n, m : integer): integer;
		var 
			result: integer;

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

		writeln(result)
	end.
	`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "MaxProgram", []string{}, 4, 1, 1, 0, 0) {
		return
	}

	value := &ast.FuncDesignator{
		Name: &ast.Identifier{TokenKind: token.Identifier, Name: "max"},
		Parameters: []ast.Expression{
			&ast.Identifier{TokenKind: token.Identifier, Name: "a"},
			&ast.Identifier{TokenKind: token.Identifier, Name: "b"},
		},
	}

	if !testAssignmentStatement(t, program.Block.Stats[2], "result", value) {
		return
	}

	if !testFuncDesignator(t, program.Block.Stats[2].(*ast.AssignStatement).Value, "max", value.Parameters) {
		return
	}
}

func TestMultipleVariableDeclarations(t *testing.T) {
	input := `
	program HelloWorld;
	var 
		a, b, sum : integer;
		c, d : integer;
		x, y, z, max : real;
		i, j : integer;
		k : 0..9;
		p, q, r : Boolean;
		operator : (plus, minus, times);
		arr : array [0..63] of real;
		date : record
			month : 1..12;
			year : integer
		end;

	begin
		writeln('Hello, world!')
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 1, 9, 0, 0, 0) {
		return
	}

	names := [][]string{
		{"a", "b", "sum"},
		{"c", "d"},
		{"x", "y", "z", "max"},
		{"i", "j"},
		{"k"},
		{"p", "q", "r"},
		{"operator"},
		{"arr"},
		{"date"},
	}
	if !testVarDeclaration(
		t,
		program.Block.VarDeclaration,
		token.NewToken(token.Var, "var"),
		9,
		names,
		[]string{"integer", "integer", "real", "integer", "subrange", "Boolean", "enum", "array", "record"},
	) {
		return
	}
}

func TestParsingMultiplicationOperator(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, c, d, e, f, sum, foo : integer;

	begin
		sum := a * b * c;
		foo := d + e * f;

		writeln('Hello, world!')
	end.
`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 3, 1, 0, 0, 0) {
		return
	}

	value := &ast.BinaryExpression{
		Operator: token.NewToken(token.Star, "*"),
		Left:     &ast.Identifier{TokenKind: token.Identifier, Name: "a"},
		Right: &ast.BinaryExpression{
			Operator: token.NewToken(token.Star, "*"),
			Left:     &ast.Identifier{TokenKind: token.Identifier, Name: "b"},
			Right:    &ast.Identifier{TokenKind: token.Identifier, Name: "c"},
		},
	}

	if !testAssignmentStatement(t, program.Block.Stats[0], "sum", value) {
		return
	}

	value = &ast.BinaryExpression{
		Operator: token.NewToken(token.Star, "+"),
		Left:     &ast.Identifier{TokenKind: token.Identifier, Name: "d"},
		Right: &ast.BinaryExpression{
			Operator: token.NewToken(token.Star, "*"),
			Left:     &ast.Identifier{TokenKind: token.Identifier, Name: "e"},
			Right:    &ast.Identifier{TokenKind: token.Identifier, Name: "f"},
		},
	}

	if !testAssignmentStatement(t, program.Block.Stats[1], "foo", value) {
		return
	}
}

func TestParsingConstantDefinition(t *testing.T) {
	input := `
	program HelloWorld;
	
	const 
		eps = 1e-10;
		pi = 3.142;
		foo = 'bar';
		baz = -45;
		c = eps;

	begin
		writeln('Hello, world!')
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 1, 2, 0, 0, 0) {
		return
	}
}

func TestParseForStatement(t *testing.T) {
	input := `
	program HelloWorld;
	var
		i, sum : integer;

	begin
		for i := 1 to 5 do
			sum := sum + i;

		writeln( sum )
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 2, 1, 0, 0, 0) {
		return
	}

	initVal := &ast.UIntegerLiteral{TokenKind: token.UIntLiteral, Value: "1"}
	finalVal := &ast.UIntegerLiteral{TokenKind: token.UIntLiteral, Value: "5"}
	body := &ast.AssignStatement{
		TokenKind: token.Initialize,
		Variable:  &ast.Identifier{TokenKind: token.Identifier, Name: "sum"},
		Value: &ast.BinaryExpression{
			Operator: token.NewToken(token.Plus, "+"),
			Left:     &ast.Identifier{TokenKind: token.Identifier, Name: "sum"},
			Right:    &ast.Identifier{TokenKind: token.Identifier, Name: "i"},
		},
	}

	stmt := &ast.ForStatement{
		TokenKind:  token.For,
		CtrlID:     &ast.Identifier{TokenKind: token.Identifier, Name: "i"},
		InitValue:  initVal,
		FinalValue: finalVal,
		Body:       body,
		Direction:  token.To,
	}

	if !testForStatement(t, stmt, "i", initVal, finalVal, token.To, body) {
		return
	}
}

func TestParsingProcedureDeclaration(t *testing.T) {
	input := `
	program HelloWorld;
	procedure bisect (function f(x : real) : real;
						      a, b         : real;
					  var     result       : real);
	const
		eps = 1e-10;
	var
		midpoint : real;
		foo : integer;
	begin
		foo := 2
	end;

	begin
		writeln('Hello, world!')
	end.
	`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 1, 2, 1, 0, 0) {
		return
	}

	procDecl := &ast.ProcedureDeclaration{
		Heading: &ast.ProcedureHeading{
			TokenKind: token.Procedure,
			Name:      &ast.Identifier{TokenKind: token.Identifier, Name: "bisect"},
			Parameters: []ast.FormalParameter{
				&ast.FuncHeading{
					TokenKind: token.Function,
					FName:     &ast.Identifier{TokenKind: token.Identifier, Name: "f"},
					Parameters: []ast.FormalParameter{
						&ast.ValueParam{
							Names: []*ast.Identifier{{TokenKind: token.Identifier, Name: "x"}},
							Type:  base.NewReal(),
						},
					},
					ReturnType: base.NewReal(),
				},
				&ast.ValueParam{
					Names: []*ast.Identifier{
						{TokenKind: token.Identifier, Name: "a"},
						{TokenKind: token.Identifier, Name: "b"},
					},
					Type: base.NewReal(),
				},
				&ast.VariableParam{
					Token: token.Var,
					Names: []*ast.Identifier{{TokenKind: token.Identifier, Name: "result"}},
					Type:  base.NewReal(),
				},
			},
		},
		Block: &ast.Block{
			Consts: &ast.ConstDefinition{
				Token: token.NewToken(token.Const, "const"),
				Consts: []*ast.ConstDef{
					{
						Name:  &ast.Identifier{TokenKind: token.Identifier, Name: "eps"},
						Value: &ast.URealLiteral{TokenKind: token.URealLiteral, Value: "1e-10"},
					},
				},
			},
			VarDeclaration: &ast.VarDeclaration{
				TokenKind: token.Var,
				Decls: []*ast.VarDecl{
					{
						Names: []*ast.Identifier{{TokenKind: token.Identifier, Name: "midpoint"}},
						Type:  base.NewReal(),
					},
				},
			},
		},
	}

	if !testProcedureDeclaration(t, procDecl, "bisect", program.Block.Callables[0].(*ast.ProcedureDeclaration)) {
		return
	}
}

func TestParseWhileStatement(t *testing.T) {
	input := `
	program HelloWorld;
	var
		i, sum : integer;

	begin
		while i < 5 do
			sum := sum + i;

		writeln( sum )
	end.
	`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 2, 1, 0, 0, 0) {
		return
	}

	if !testWhileStatement(t, program.Block.Stats[0]) {
		return
	}
}

func testWhileStatement(t *testing.T, stmt ast.Statement) bool {
	whileStmt, ok := stmt.(*ast.WhileStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.WhileStatement; found %v", whileStmt)
		return false
	}

	if whileStmt.TokenKind != token.While {
		t.Errorf("expected token to kind 'while', got '%v' instead.", whileStmt.TokenKind)
		return false
	}

	testBinaryExpression(
		t, whileStmt.BoolExpr, token.Token{Kind: token.LessThan, Text: "<"}, "i", "5")

	return true
}

func testReturnStatement(t *testing.T, stmt ast.Statement, expr string) bool {
	retStmt, ok := stmt.(*ast.ReturnStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.ReturnStatement; found %v", retStmt)
		return false
	}

	if retStmt.TokenKind != token.Return {
		t.Errorf("expected token to be of kind %v, got %v", token.Return, retStmt.TokenKind)
		return false
	}

	if retStmt.Expr.String() != expr {
		t.Errorf("expected initial value to be %s, got %v instead", expr, retStmt.Expr.String())
		return false
	}

	return true
}

func testForStatement(
	t *testing.T,
	stmt ast.Statement,
	ctrlVar string,
	initValue, finalValue ast.Expression,
	direction token.Kind,
	body ast.Statement,
) bool {
	forStmt, ok := stmt.(*ast.ForStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.ForStatement; found %v", forStmt)
		return false
	}

	if forStmt.CtrlID.TokenKind != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.Identifier, forStmt.CtrlID.TokenKind)
		return false
	}

	if forStmt.CtrlID.String() != ctrlVar {
		t.Errorf("expected assignment value to be %v. got %v instead",
			ctrlVar, forStmt.CtrlID)

		return false
	}

	if forStmt.InitValue.String() != initValue.String() {
		t.Errorf("expected initial value to be %s, got %v instead",
			initValue.String(), forStmt.InitValue.String())
		return false
	}

	if forStmt.FinalValue.String() != finalValue.String() {
		t.Errorf("expected final value to be %s, got %v instead",
			finalValue.String(), forStmt.FinalValue.String())
		return false
	}

	if forStmt.Direction != direction {
		t.Errorf("expected loop iteration direction to %s, got %s instead",
			direction, forStmt.Direction)

		return false
	}

	//if forStmt.Body.StatNode() != body.StatNode() {
	//	t.Errorf("expected body to be %s, got %s instead", body.StatNode(), forStmt.Body.StatNode())
	//	return false
	//}

	return true
}

func testProcedureDeclaration(
	t *testing.T,
	pd ast.Statement,
	procedureName string,
	programPD *ast.ProcedureDeclaration,
) bool {
	procDecl, ok := pd.(*ast.ProcedureDeclaration)
	if !ok {
		t.Errorf("expected procedure declaration type, got %v", procDecl)
		return false
	}

	if procDecl.Heading.TokenKind != token.Procedure {
		t.Errorf("procedure declaration has wrong token type, %v", procDecl.Heading.TokenKind)
		return false
	}

	if procDecl.Heading.Name.String() != procedureName {
		t.Errorf("expected procedure name to be %v, got %v instead", procedureName, procDecl.Heading.Name.String())
		return false
	}

	paramList := programPD.Heading.Parameters
	for i, j := 0, 0; i < len(paramList) && j < len(procDecl.Heading.Parameters); i, j = i+1, j+1 {
		if paramList[i].String() != procDecl.Heading.Parameters[j].String() {
			t.Errorf("expected parameter type to be %v, got %v instead",
				procDecl.Heading.Parameters[j].String(), paramList[i].String())

			return false
		}
	}

	numVarDecl := 0
	if procDecl.Block.VarDeclaration != nil {
		numVarDecl = len(procDecl.Block.VarDeclaration.Decls)
	}

	numTypeDef := 0
	if procDecl.Block.Types != nil {
		numTypeDef = len(procDecl.Block.Types.Types)
	}

	numLabels := 0
	if procDecl.Block.Labels != nil {
		numLabels = len(procDecl.Block.Labels.Labels)
	}

	if !testBlock(
		t,
		procDecl.Block,
		len(procDecl.Block.Stats),
		numVarDecl,
		len(procDecl.Block.Callables),
		numTypeDef, numLabels) {
		return false
	}

	return true
}

func testAssignmentStatement(t *testing.T, stmt ast.Statement, variable string, value ast.Expression) bool {
	assignStmt, ok := stmt.(*ast.AssignStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.AssignStatement; found %v", assignStmt)
		return false
	}

	if assignStmt.Variable.String() != variable {
		t.Errorf("expected variable to be '%v', got '%v' instead,", variable, assignStmt.Variable.String())
		return false
	}

	if assignStmt.Value.String() != value.String() {
		t.Errorf("expected assignment value to be '%v'. got '%v' instead",
			value.String(), assignStmt.Value.String())

		return false
	}

	return true
}

func testProgramAST(
	t *testing.T, p *ast.Program, programName string, paramList []string, numStmts, numVarDefs, numCallables, numTypeDefs, numLabels int,
) bool {
	if p == nil {
		t.Error("AST not created")
		return false
	}

	if p.Name.String() != programName {
		t.Errorf("expected program name to be %v, got %v instead", programName, p.Name.String())
		return false
	}

	if len(p.ParamList) != len(paramList) {
		t.Errorf("expected %v program parameters, got %v instead", len(paramList), len(p.ParamList))
		return false
	}

	for i, j := 0, 0; i < len(p.ParamList) && j < len(paramList); i, j = i+1, j+1 {
		if p.ParamList[i].String() != paramList[j] {
			t.Errorf("expected var %v, got %v instead", p.ParamList[i].String(), paramList[j])
			return false
		}
	}

	return testBlock(t, p.Block, numStmts, numVarDefs, numCallables, numTypeDefs, numLabels)
}

func testBlock(t *testing.T, blk *ast.Block, numStmts, numVarDefs, numCallables, numTypeDefs, numLabels int) bool {
	if len(blk.Stats) != numStmts {
		t.Errorf("expected %v statement(s) in block; found %v instead", numStmts, len(blk.Stats))
		return false
	}

	if blk.VarDeclaration != nil && len(blk.VarDeclaration.Decls) != numVarDefs {
		t.Errorf("expected %v variable declaration(s) in block; found %v instead", numVarDefs, len(blk.VarDeclaration.Decls))
		return false
	}

	if blk.Callables != nil && len(blk.Callables) != numCallables {
		t.Errorf("expected %v callable(s) in block; found %v instead", numCallables, len(blk.Callables))
		return false
	}

	if blk.Types != nil && len(blk.Types.Types) != numTypeDefs {
		t.Errorf("expected %v type definition(s) in block; found %v instead", numTypeDefs, len(blk.Types.Types))
		return false
	}

	if blk.Labels != nil && len(blk.Labels.Labels) != numLabels {
		t.Errorf("expected %v label declarations(s) in block; found %v instead", numLabels, len(blk.Labels.Labels))
		return false
	}

	return true
}

func testFuncDesignator(t *testing.T, funcDesg ast.Expression, funcName string, params []ast.Expression) bool {
	fDesg, ok := funcDesg.(*ast.FuncDesignator)
	if !ok {
		t.Errorf("expected a function designator, instead got %v", funcDesg)
		return false
	}

	if fDesg.Name.Name != funcName {
		t.Errorf("expected function designator name to be %v, got %v instead", funcName, fDesg.Name.Name)
		return false
	}

	for i, j := 0, 0; i < len(params) && j < len(fDesg.Parameters); i, j = i+1, j+1 {
		if params[i].String() != fDesg.Parameters[j].String() {
			t.Errorf("expected parameter %v, got %v instead",
				params[i].String(), fDesg.Parameters[j].String())
			return false
		}
	}

	return true
}

func testBinaryExpression(t *testing.T, expr ast.Expression, operator token.Token, left, right string) bool {
	exp, ok := expr.(*ast.BinaryExpression)
	if !ok {
		t.Errorf("expected binary expresion, got %v", exp)
		return false
	}

	if exp.Operator.Kind != operator.Kind {
		t.Errorf("expected operator %v, got %v instead", operator.Text, exp.Operator.Text)
		return false
	}

	if exp.Left.String() != left {
		t.Errorf("expected LHS to be %v, got %v instead", left, exp.Left)
		return false
	}

	if exp.Right.String() != right {
		t.Errorf("expected RHS to be %v, got %v instead", right, exp.Right)
		return false
	}

	return true
}

func testFuncDeclaration(
	t *testing.T,
	fd ast.Statement,
	funcName, retType string,
	paramList []ast.FormalParameter,
	numStmts, numVarDefs, numCallables, numTypeDefs, numLabels int,
) bool {
	funcDecl, ok := fd.(*ast.FuncDeclaration)
	if !ok {
		t.Errorf("expected function declaration type, got %v", funcDecl)
		return false
	}

	if funcDecl.Heading.TokenKind != token.Function {
		t.Errorf("function declaration has wrong token type, %v", funcDecl.Heading.TokenKind)
		return false
	}

	if funcDecl.Heading.FName.Name != funcName {
		t.Errorf("expected function name to be %v, got %v instead", funcName, funcDecl.Heading.FName)
		return false
	}

	if funcDecl.Heading.ReturnType.Name() != retType {
		t.Errorf("expected return type to be %v, got %v instead", retType, funcDecl.Heading.ReturnType)
	}

	for i, j := 0, 0; i < len(paramList) && j < len(funcDecl.Heading.Parameters); i, j = i+1, j+1 {
		if paramList[i].String() != funcDecl.Heading.Parameters[j].String() {
			t.Errorf("expected parameter type to be %v, got %v instead",
				paramList[i].String(), funcDecl.Heading.Parameters[j].String())

			return false
		}
	}

	if !testBlock(t, funcDecl.Block, numStmts, numVarDefs, numCallables, numTypeDefs, numLabels) {
		return false
	}

	return true
}

func testIfStatement(
	t *testing.T,
	stmt ast.Statement,
	bExprOp token.Token,
	left, right string,
	tPath, elsePath string,
) bool {
	ifStmt, ok := stmt.(*ast.IfStatement)
	if !ok {
		t.Errorf("expected if statement, got %v", ifStmt)
		return false
	}

	if ifStmt.TokenKind != token.If {
		t.Errorf("expected token type to be %v, got %v", token.If, ifStmt.TokenKind)
		return false
	}

	if !testBinaryExpression(t, ifStmt.BoolExpr, bExprOp, left, right) {
		return false
	}

	if ifStmt.TruePath.String() != tPath {
		t.Errorf("expected true path to be %v, got %v instead", tPath, ifStmt.TruePath.String())
		return false
	}

	if ifStmt.ElsePath.String() != elsePath {
		t.Errorf("expected else path to be %v, got %v instead", elsePath, ifStmt.ElsePath.String())
	}

	return true
}

func testUnaryExpression(t *testing.T, expr ast.Expression, operator token.Token, operand string) bool {
	uexpr, ok := expr.(*ast.UnaryExpression)
	if !ok {
		t.Errorf("expected RHS of to be ast.UnaryExpression, got %v instead", uexpr)
		return false
	}

	if uexpr.Operator.Kind != operator.Kind {
		t.Errorf("expected operator %v, got %v instead", operator.Text, uexpr.Operator.Text)
		return false
	}

	if uexpr.Operand.String() != operand {
		t.Errorf("expected operand to be %v, got %v instead", operand, uexpr.String())
		return false
	}

	return true
}

func testVarDeclaration(
	t *testing.T, vd *ast.VarDeclaration, tt token.Token, varDeclCount int, varList [][]string, varType []string,
) bool {
	if vd.TokenKind != tt.Kind {
		t.Errorf("expected token to be %v; got %v instead", tt.Text, vd.TokenKind)
		return false
	}

	if len(vd.Decls) != varDeclCount {
		t.Errorf("expected %v variables; got %v", varDeclCount, len(vd.Decls))
		return false
	}

	for idx, decl := range vd.Decls {
		names := varList[idx]

		for i, j := 0, 0; i < len(decl.Names) && j < len(names); i, j = i+1, j+1 {
			if decl.Names[i].String() != names[j] {
				t.Errorf("expected var %v, got %v instead", decl.Names[i].String(), names[j])
				return false
			}
		}

		if decl.Type.Name() != varType[idx] {
			t.Errorf("expected variable type to be %v, got %v instead", varType[idx], decl.Type)
			return false
		}
	}

	return true
}

func TestParseRepeatStatement(t *testing.T) {
	input := `
	program HelloWorld;
	var
		i, j, k, sum : integer;

	begin
		repeat
			k := i mod j;
			i := j;
			j := k
		until j = 0;			

		writeln( sum )
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 2, 1, 0, 0, 0) {
		return
	}

	if !testRepeatStatement(t, program.Block.Stats[0]) {
		return
	}
}

func testRepeatStatement(t *testing.T, stmt ast.Statement) bool {
	repeatStmt, ok := stmt.(*ast.RepeatStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.RepeatStatement; found %v", repeatStmt)
		return false
	}

	testBinaryExpression(
		t, repeatStmt.BoolExpr, token.NewToken(token.Equal, "="), "j", "0")

	return true
}

func TestParseTypeDefinitionPart(t *testing.T) {
	input := `
	program HelloWorld;

	type
		count = integer;
		range = integer;
		colour = (red, yellow, green, blue);
		sex = (male, female);
		year = 1900..1999;
		volume = -10..+10;
		list = array [1..100] of real;
		vector = array [year] of integer;
		arr = array [Boolean] of colour;
		punchedcard = array [1..80] of char;
		uniq = set of char;
		cards = set of (club, diamond, heart, spade);	
		polar = record
			r : real;
			theta : volume
		end;
		persondetails = record
			name, firstname : integer;
			age : integer;
			married : Boolean;
			father, child, sibling : uniq;
			case s : sex of
			male :
				(enlisted, bearded : Boolean);
			female :
				(mother, programmer : Boolean)
		end;
		person = ^ persondetails;

	var 
		a, b, sum : integer;

	begin
		writeln('Hello, world!')
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 1, 1, 0, 15, 0) {
		return
	}
}

func TestParsingIndexedVariables(t *testing.T) {
	input := `
	program HelloWorld;
	type
		pc = array [1..80] of char;

	var
		a, b, sum : integer;

	begin
		pc[0] := 1;
		a := 17;
		pc[5] := 2;
		pc[a+b] := 2;

		writeln('Hello, world!')
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 5, 1, 0, 1, 0) {
		return
	}

	val := &ast.Identifier{TokenKind: token.UIntLiteral, Name: "1"}
	if !testAssignmentStatement(t, program.Block.Stats[0], "pc[0]", val) {
		return
	}

	val = &ast.Identifier{TokenKind: token.UIntLiteral, Name: "2"}
	if !testAssignmentStatement(t, program.Block.Stats[2], "pc[5]", val) {
		return
	}

	if !testAssignmentStatement(t, program.Block.Stats[3], "pc[a + b]", val) {
		return
	}
}

func TestParseExpressions(t *testing.T) {
	input := `
	program HelloWorld;
	
	type
		hue1 = set of integer;
		hue2 = set of integer;

	var
		a, b, c, i, j, k, red, green : integer;
		x, y, z : real;
		p, q, r : Boolean;

	function 
		sin(n: integer): real;
		begin
			
		end;

	begin
		a := 15;
		a := (x + y + z);
		a := sin(x + y);
		hue1 := [red, c, green];
		hue2 := [1, 5, 10..19, 23];
		a := not p;

		b := x * y;
		b := i / (1 - i);
		b := (x <= y) and (y < z);

		c := p or q;
		c := x + y;
		c := -x;
		c := hue1 + hue2;
		c := i * j + 1;

		x := 1.5;
		r := p <= q;
		r := p = q and r;
		r := (i < j) = (j < k);
		r := c in hue1;

		writeln('Hello, world!')
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 20, 3, 1, 2, 0) {
		return
	}
}

func TestParseFieldDesignator(t *testing.T) {
	input := `
	program HelloWorld;

	type
		sex = (male, female);
		person = record
			name, firstname : integer;
			age : integer;
			married : Boolean;
			case s : sex of
			male :
				(enlisted, bearded : Boolean);
			female :
				(mother, programmer : Boolean)
		end;

	var
		a, b, sum : integer;
		p2        : ^person;

	begin
		person.firstname := 1;
        p2^.married := true;

		writeln( person.firstname )
	end.
`
	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 3, 2, 0, 2, 0) {
		return
	}

	val := &ast.Identifier{TokenKind: token.Identifier, Name: "1"}
	if !testAssignmentStatement(t, program.Block.Stats[0], "person.firstname", val) {
		return
	}
}

func TestParserArrayType(t *testing.T) {
	input := `
		program HelloWorld;
		
		type
			size = integer;
			colour = (red, yellow, green, blue);

			arr = array [1..100] of real;
			kar = array [Boolean] of colour;
			foo = array [Boolean] of array [1..10] of array [size] of real;
			bar = array [Boolean] of array [1..10, size] of real;
			baz = array [Boolean, 1..10, size] of real;
			bam = array [Boolean, 1..10] of array [size] of real;

		begin
			writeln('Hello, world!')
		end.
	`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "HelloWorld", []string{}, 1, 1, 0, 8, 0) {
		return
	}
}

func TestParseLabelDeclaration(t *testing.T) {
	input := `
		program LabelProgram;

		label
			4833, 9999, 0, 2, 921;

		begin
			writeln('Hello, world!')
		end.
	
	`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "LabelProgram", []string{}, 1, 0, 0, 8, 5) {
		return
	}
}

func TestParseStatementsWithLabels(t *testing.T) {
	input := `
		program LabelProgram;

		label
			4833, 9999, 0, 2, 921;

		var
			a, b, sum : integer;

		begin
			9999:
				begin
					a := 1;
					b := 2;
					writeln('Hello, world!')
				end;

			921:	
				a := 1;

			writeln('Hello, world!')
		end.
	
	`

	lex := NewLexer(input)
	symTable := semantics.NewWonkySymbolTable()
	pars, err := NewParser(lex, symTable)
	if err != nil {
		t.Error(err)
	}

	program, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, program, "LabelProgram", []string{}, 3, 1, 0, 0, 5) {
		return
	}
}
