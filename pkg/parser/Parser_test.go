package parser

import (
	"testing"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/symbols"
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 0, 0, 0) {
		return
	}

	args := []ast.Expression{
		&ast.CharString{Token: token.NewToken(token.CharString, "Hello, World!"), Value: "Hello, World!"},
	}
	if !testWriteln(t, prog.Block.Stats[0], "writeln", args) {
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 1, 0, 0) {
		return
	}

	if !testVarDeclaration(
		t, prog.Block.VarDeclaration, token.NewToken(token.Var, "var"), 1,
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 3, 1, 0, 0) {
		return
	}

	// first statement
	value := &ast.UIntegerLiteral{Token: token.Token{Text: "1", Kind: token.UIntLiteral}, Value: "1"}
	if !testAssignmentStatement(t, prog.Block.Stats[0], "a", value) {
		return
	}

	// second statement
	value = &ast.UIntegerLiteral{Token: token.Token{Text: "2", Kind: token.UIntLiteral}, Value: "2"}
	if !testAssignmentStatement(t, prog.Block.Stats[1], "b", value) {
		return
	}

	// third statement
	args := []ast.Expression{
		&ast.CharString{Token: token.NewToken(token.CharString, "Hello, world!"), Value: "Hello, world!"},
	}

	if !testWriteln(t, prog.Block.Stats[2], "writeln", args) {
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 5, 1, 0, 0) {
		return
	}

	expr := &ast.BinaryExpression{
		Operator: token.NewToken(token.Plus, "+"),
		Left:     &ast.Identifier{Token: token.NewToken(token.Identifier, "a"), Name: "a"},
		Right:    &ast.Identifier{Token: token.NewToken(token.Identifier, "b"), Name: "b"},
	}

	if !testAssignmentStatement(t, prog.Block.Stats[2], "sum", expr) {
		return
	}

	stmt := prog.Block.Stats[2].(*ast.AssignStatement)
	if !testBinaryExpression(t, stmt.Value, expr.Operator, "a", "b") {
		return
	}

	uexpr := &ast.UnaryExpression{
		Operator: token.NewToken(token.Minus, "-"),
		Operand:  &ast.UIntegerLiteral{Token: token.NewToken(token.UIntLiteral, "5"), Value: "5"},
	}
	if !testAssignmentStatement(t, prog.Block.Stats[3], "a", uexpr) {
		return
	}

	stmt = prog.Block.Stats[3].(*ast.AssignStatement)
	if !testUnaryExpression(t, stmt.Value, token.NewToken(token.Minus, "-"), "-5") {
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "MaxProgram", []string{}, 3, 1, 1, 0) {
		return
	}

	params := []ast.FormalParameter{
		&ast.ValueParam{
			Names: []*ast.Identifier{
				{Token: token.NewToken(token.Identifier, "n"), Name: "n"},
				{Token: token.NewToken(token.Identifier, "m"), Name: "m"},
			},
			Type: &base.Integer{Name: "integer"},
		},
	}
	if !testFuncDeclaration(t, prog.Block.Callables[0], "foo", "integer", params, 1, 1, 0, 0) {
		return
	}

	funcDecl := prog.Block.Callables[0].(*ast.FuncDeclaration)
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "MaxProgram", []string{}, 3, 1, 1, 0) {
		return
	}

	paramList := []ast.FormalParameter{
		&ast.ValueParam{
			Names: []*ast.Identifier{
				{Token: token.NewToken(token.Identifier, "n"), Name: "n"},
				{Token: token.NewToken(token.Identifier, "m"), Name: "m"},
			},
			Type: &base.Integer{Name: "integer"},
		},
	}
	if !testFuncDeclaration(t, prog.Block.Callables[0], "max", "integer", paramList, 2, 1, 0, 0) {
		return
	}

	funcDecl := prog.Block.Callables[0].(*ast.FuncDeclaration)
	if !testIfStatement(t, funcDecl.Block.Stats[0], token.NewToken(token.GreaterThan, ">"), "n", "m", "result := n", "result := m") {
		return
	}

	ifStmt := funcDecl.Block.Stats[0].(*ast.IfStatement)
	value := &ast.Identifier{
		Token: token.NewToken(token.Identifier, "n"),
		Name:  "n",
	}
	if !testAssignmentStatement(t, ifStmt.TruePath, "result", value) {
		return
	}

	value.Token.Text = "m"
	value.Name = "m"

	if !testAssignmentStatement(t, ifStmt.ElsePath, "result", value) {
		return
	}
}

func TestParseProgramWithFunctionCall(t *testing.T) {
	input := `
	program MaxProgram;
	var
		a, b : integer;
	
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "MaxProgram", []string{}, 4, 1, 1, 0) {
		return
	}

	value := &ast.FuncDesignator{
		Name: &ast.Identifier{Token: token.Token{Kind: token.Identifier, Text: "max"}, Name: "max"},
		Parameters: []ast.Expression{
			&ast.Identifier{Token: token.Token{Kind: token.Identifier, Text: "a"}, Name: "a"},
			&ast.Identifier{Token: token.Token{Kind: token.Identifier, Text: "b"}, Name: "b"},
		},
	}

	if !testAssignmentStatement(t, prog.Block.Stats[2], "result", value) {
		return
	}

	if !testFuncDesignator(t, prog.Block.Stats[2].(*ast.AssignStatement).Value, "max", value.Parameters) {
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 9, 0, 0) {
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
		prog.Block.VarDeclaration,
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 3, 1, 0, 0) {
		return
	}

	value := &ast.BinaryExpression{
		Operator: token.NewToken(token.Star, "*"),
		Left:     &ast.Identifier{Token: token.NewToken(token.Identifier, "a"), Name: "a"},
		Right: &ast.BinaryExpression{
			Operator: token.NewToken(token.Star, "*"),
			Left:     &ast.Identifier{Token: token.NewToken(token.Identifier, "b"), Name: "b"},
			Right:    &ast.Identifier{Token: token.NewToken(token.Identifier, "c"), Name: "c"},
		},
	}

	if !testAssignmentStatement(t, prog.Block.Stats[0], "sum", value) {
		return
	}

	value = &ast.BinaryExpression{
		Operator: token.NewToken(token.Star, "+"),
		Left:     &ast.Identifier{Token: token.NewToken(token.Identifier, "d"), Name: "d"},
		Right: &ast.BinaryExpression{
			Operator: token.NewToken(token.Star, "*"),
			Left:     &ast.Identifier{Token: token.NewToken(token.Identifier, "e"), Name: "e"},
			Right:    &ast.Identifier{Token: token.NewToken(token.Identifier, "f"), Name: "f"},
		},
	}

	if !testAssignmentStatement(t, prog.Block.Stats[1], "foo", value) {
		return
	}
}

func TestSymbolTableGenerated(t *testing.T) {
	input := `
	program MaxProgram;
	var 
		a, b, sum : integer;

	function max(n, m :integer): integer;
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

		writeln(result)
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

	if !testProgramAST(t, prog, "MaxProgram", []string{}, 4, 1, 1, 0) {
		return
	}

	symTable := pars.SymbolTable()
	if !testGlobalSymbolTable(t, symTable, nil, "global", len(symTable.Symbols)) {
		return
	}

	max := symTable.Resolve("max")
	if max.GetKind() != symbols.FUNCTION {
		t.Errorf("expected symbol of kind %v, got %v instead", symbols.FUNCTION, max.GetKind())
	}

	m, ok := max.(*symbols.Function)
	if !ok || m.Scope == nil {
		t.Errorf("expected a function symbol type")
	}

	if !testLocalSymbolTable(t, m.Scope, symTable, "max", 3) {
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 2, 0, 0) {
		return
	}

	if !testGlobalSymbolTable(t, pars.symTable, nil, "global", 11) {
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 2, 1, 0, 0) {
		return
	}

	initVal := &ast.UIntegerLiteral{Token: token.NewToken(token.UIntLiteral, "1"), Value: "1"}
	finalVal := &ast.UIntegerLiteral{Token: token.NewToken(token.UIntLiteral, "5"), Value: "5"}
	body := &ast.AssignStatement{
		Token:    token.NewToken(token.Initialize, ":="),
		Variable: &ast.Identifier{Token: token.NewToken(token.Identifier, "sum")},
		Value: &ast.BinaryExpression{
			Operator: token.NewToken(token.Plus, "+"),
			Left:     &ast.Identifier{Token: token.NewToken(token.Identifier, "sum")},
			Right:    &ast.Identifier{Token: token.NewToken(token.Identifier, "i")},
		},
	}

	stmt := &ast.ForStatement{
		Token:      token.NewToken(token.For, "for"),
		CtrlID:     &ast.Identifier{Token: token.NewToken(token.Identifier, "i")},
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 2, 1, 0) {
		return
	}

	procDecl := &ast.ProcedureDeclaration{
		Heading: &ast.ProcedureHeading{
			Token: token.NewToken(token.Procedure, "procedure"),
			Name:  &ast.Identifier{Token: token.NewToken(token.Identifier, "bisect")},
			Parameters: []ast.FormalParameter{
				&ast.FuncHeading{
					Token: token.NewToken(token.Function, "function"),
					Name:  &ast.Identifier{Token: token.NewToken(token.Identifier, "f"), Name: "f"},
					Parameters: []ast.FormalParameter{
						&ast.ValueParam{
							Names: []*ast.Identifier{{Token: token.NewToken(token.Identifier, "x"), Name: "x"}},
							Type:  &base.Real{Name: "real"},
						},
					},
					ReturnType: &base.Real{Name: "real"},
				},
				&ast.ValueParam{
					Names: []*ast.Identifier{
						{Token: token.NewToken(token.Identifier, "a"), Name: "a"},
						{Token: token.NewToken(token.Identifier, "b"), Name: "b"},
					},
					Type: &base.Real{Name: "real"},
				},
				&ast.VariableParam{
					Token: token.Var,
					Names: []*ast.Identifier{{Token: token.NewToken(token.Identifier, "result"), Name: "result"}},
					Type:  &base.Real{Name: "real"},
				},
			},
		},
		Block: &ast.Block{
			Consts: &ast.ConstDefinition{
				Token: token.NewToken(token.Const, "const"),
				Consts: []*ast.ConstDef{
					{
						Name:  &ast.Identifier{Token: token.NewToken(token.Identifier, "eps"), Name: "eps"},
						Value: &ast.URealLiteral{Token: token.NewToken(token.URealLiteral, "real"), Value: "1e-10"},
					},
				},
			},
			VarDeclaration: &ast.VarDeclaration{
				Token: token.NewToken(token.Var, "var"),
				Decls: []*ast.VarDecl{
					{
						Names: []*ast.Identifier{{Token: token.NewToken(token.Identifier, "midpoint"), Name: "real"}},
						Type:  &base.Real{Name: "real"},
					},
				},
			},
		},
	}

	if !testProcedureDeclaration(t, procDecl, "bisect", prog.Block.Callables[0].(*ast.ProcedureDeclaration)) {
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 2, 1, 0, 0) {
		return
	}

	if !testWhileStatement(t, prog.Block.Stats[0]) {
		return
	}
}

func testWhileStatement(t *testing.T, stmt ast.Statement) bool {
	whileStmt, ok := stmt.(*ast.WhileStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.WhileStatement; found %v", whileStmt)
		return false
	}

	if whileStmt.Token.Kind != token.While {
		t.Errorf("expected token to kind 'while', got '%v' instead.", whileStmt.Token.Text)
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

	if retStmt.TokenKind() != token.Return {
		t.Errorf("expected token to be of kind %v, got %v", token.Return, retStmt.TokenKind())
		return false
	}

	if retStmt.TokenLiteral() != "return" {
		t.Errorf("expected token literal to be 'return', got %v", retStmt.TokenLiteral())
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

	if forStmt.CtrlID.TokenKind() != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.Identifier, forStmt.CtrlID.Token.Kind)
		return false
	}

	if forStmt.CtrlID.TokenLiteral() != ctrlVar {
		t.Errorf("expected assignment value to be %v. got %v instead",
			ctrlVar, forStmt.CtrlID.TokenLiteral())

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

	if forStmt.Body.StatNode() != body.StatNode() {
		t.Errorf("expected body to be %s, got %s instead", body.StatNode(), forStmt.Body.StatNode())
		return false
	}

	return true
}

func testProcedureDeclaration(
	t *testing.T,
	pd ast.Statement,
	procedureName string,
	progPD *ast.ProcedureDeclaration,
) bool {
	procDecl, ok := pd.(*ast.ProcedureDeclaration)
	if !ok {
		t.Errorf("expected procedure declaration type, got %v", procDecl)
		return false
	}

	if procDecl.Heading.Token.Kind != token.Procedure {
		t.Errorf("procedure declaration has wrong token type, %v", procDecl.Heading.Token.Text)
		return false
	}

	if procDecl.Heading.Name.TokenLiteral() != procedureName {
		t.Errorf("expected procedure name to be %v, got %v instead", procedureName, procDecl.Heading.Name.TokenLiteral())
		return false
	}

	paramList := progPD.Heading.Parameters
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

	if !testBlock(
		t,
		procDecl.Block,
		len(procDecl.Block.Stats),
		numVarDecl,
		len(procDecl.Block.Callables),
		numTypeDef) {
		return false
	}

	return true
}

func testGlobalSymbolTable(
	t *testing.T, symTable, parentScope symbols.Scope, scopeName string, numSymbols int,
) bool {
	if symTable == nil {
		t.Errorf("symbol table is nil")
		return false
	}

	if symTable.GetEnclosingScope() != parentScope {
		t.Errorf("enclosing scopes do not match")
		return false
	}

	if symTable.GetScopeName() != scopeName {
		t.Errorf("expected scope name to be %v, got %v instead", scopeName, symTable.GetScopeName())
		return false
	}

	symTab, ok := symTable.(*symbols.GlobalScope)
	if !ok {
		t.Errorf("expected a global symbol table")
		return false
	}

	if len(symTab.Symbols) != numSymbols {
		t.Errorf("expetecd global scope to have %v entries, got %v instead",
			numSymbols, len(symTab.Symbols))
		return false
	}

	return true
}

func testLocalSymbolTable(
	t *testing.T, symTable, parentScope symbols.Scope, scopeName string, numSymbols int,
) bool {
	if symTable == nil {
		t.Errorf("symbol table is nil")
		return false
	}

	if symTable.GetEnclosingScope() != parentScope {
		t.Errorf("enclosing scopes do not match")
		return false
	}

	if symTable.GetScopeName() != scopeName {
		t.Errorf("expected scope name to be %v, got %v instead", scopeName, symTable.GetScopeName())
		return false
	}

	symTab, ok := symTable.(*symbols.LocalScope)
	if !ok {
		t.Errorf("expected a global symbol table")
		return false
	}

	if len(symTab.Symbols) != numSymbols {
		t.Errorf("expetecd global scope to have %v entries, got %v instead",
			numSymbols, len(symTab.Symbols))
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

	if assignStmt.Variable.TokenKind() != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.Identifier, assignStmt.Variable.TokenKind())
		return false
	}

	if assignStmt.Variable.String() != variable {
		t.Errorf("expected variable to be %v, got %v instead,", variable, assignStmt.Variable.String())
		return false
	}

	if assignStmt.Value.TokenLiteral() != value.TokenLiteral() {
		t.Errorf("expected assignment value to be %v. got %v instead",
			value.TokenLiteral(), assignStmt.Value.TokenLiteral())

		return false
	}

	return true
}

func testProgramAST(
	t *testing.T, p *ast.Program, progName string, paramList []string, numStmts, numVarDefs, numCallables, numTypeDefs int,
) bool {
	if p == nil {
		t.Error("AST not created")
		return false
	}

	if p.Name.String() != progName {
		t.Errorf("expected program name to be %v, got %v instead", progName, p.Name.String())
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

	return testBlock(t, p.Block, numStmts, numVarDefs, numCallables, numTypeDefs)
}

func testBlock(t *testing.T, blk *ast.Block, numStmts, numVarDefs, numCallables, numTypeDefs int) bool {
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
		if params[i].TokenLiteral() != fDesg.Parameters[j].TokenLiteral() {
			t.Errorf("expected parameter %v, got %v instead",
				params[i].TokenLiteral(), fDesg.Parameters[j].TokenLiteral())
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

	if exp.Left.TokenLiteral() != left {
		t.Errorf("expected LHS to be %v, got %v instead", left, exp.Left.TokenLiteral())
		return false
	}

	if exp.Right.TokenLiteral() != right {
		t.Errorf("expected RHS to be %v, got %v instead", right, exp.Right.TokenLiteral())
		return false
	}

	return true
}

func testFuncDeclaration(
	t *testing.T,
	fd ast.Statement,
	funcName, retType string,
	paramList []ast.FormalParameter,
	numStmts, numVarDefs, numCallables, numTypeDefs int,
) bool {
	funcDecl, ok := fd.(*ast.FuncDeclaration)
	if !ok {
		t.Errorf("expected function declaration type, got %v", funcDecl)
		return false
	}

	if funcDecl.Heading.Token.Kind != token.Function {
		t.Errorf("function declaration has wrong token type, %v", funcDecl.Heading.Token.Text)
		return false
	}

	if funcDecl.Heading.Name.TokenLiteral() != funcName {
		t.Errorf("expected function name to be %v, got %v instead", funcName, funcDecl.Heading.Name.TokenLiteral())
		return false
	}

	if funcDecl.Heading.ReturnType.GetName() != retType {
		t.Errorf("expected return type to be %v, got %v instead", retType, funcDecl.Heading.ReturnType.GetName())
	}

	for i, j := 0, 0; i < len(paramList) && j < len(funcDecl.Heading.Parameters); i, j = i+1, j+1 {
		if paramList[i].String() != funcDecl.Heading.Parameters[j].String() {
			t.Errorf("expected parameter type to be %v, got %v instead",
				paramList[i].String(), funcDecl.Heading.Parameters[j].String())

			return false
		}
	}

	if !testBlock(t, funcDecl.Block, numStmts, numVarDefs, numCallables, numTypeDefs) {
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

	if ifStmt.Token.Kind != token.If {
		t.Errorf("expected token type to be %v, got %v", token.If, ifStmt.Token.Text)
		return false
	}

	if !testBinaryExpression(t, ifStmt.BoolExpr, bExprOp, left, right) {
		return false
	}

	if ifStmt.TruePath.StatNode() != tPath {
		t.Errorf("expected true path to be %v, got %v instead", tPath, ifStmt.TruePath.TokenLiteral())
		return false
	}

	if ifStmt.ElsePath.StatNode() != elsePath {
		t.Errorf("expected else path to be %v, got %v instead", elsePath, ifStmt.ElsePath.TokenLiteral())
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

	if uexpr.String() != operand {
		t.Errorf("expected operand to be %v, got %v instead", operand, uexpr.String())
		return false
	}

	return true
}

func testProcedureStatement(t *testing.T, stmt ast.Statement, procName string, args []ast.Expression) bool {
	procStat, ok := stmt.(*ast.ProcedureStmt)
	if !ok {
		t.Errorf("expected statement of type, ast.procedure_stmt; found %v", procStat)
	}

	if procStat.Name.String() != procName {
		t.Errorf("expected procedure name %v, got %v instead", procName, procStat.Name.String())
		return false
	}

	for i, j := 0, 0; i < len(args) && j < len(procStat.ParamList); i, j = i+1, j+1 {
		if args[i].TokenLiteral() != procStat.ParamList[j].TokenLiteral() {
			t.Errorf("expected parameter %v, got %v instead",
				args[i].TokenLiteral(), procStat.ParamList[j].TokenLiteral())
			return false
		}
	}

	return true
}

func testVarDeclaration(
	t *testing.T, stmt ast.Statement, tt token.Token, varDeclCount int, varList [][]string, varType []string,
) bool {
	vd, ok := stmt.(*ast.VarDeclaration)
	if !ok {
		t.Errorf("expected variable declaration type, got %v instead", vd)
	}

	if vd.Token.Kind != tt.Kind {
		t.Errorf("expected token to be %v; got %v instead", tt.Text, vd.Token.Text)
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

		if decl.Type.GetName() != varType[idx] {
			t.Errorf("expected variable type to be %v, got %v instead", varType[idx], decl.Type.GetName())
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 2, 1, 0, 0) {
		return
	}

	if !testRepeatStatement(t, prog.Block.Stats[0]) {
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

	var 
		a, b, sum : integer;

	begin
		writeln('Hello, world!')
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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 1, 0, 14) {
		return
	}
}

func TestParsingIndexedVariables(t *testing.T) {
	input := `
	program HelloWorld;
	type
		punchedcard = array [1..80] of char;

	var
		a, b, sum : integer;

	begin
		punchedcard[0] := 1;
		a := 17;
		punchedcard[5] := 2;

		writeln('Hello, world!')
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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 4, 1, 0, 1) {
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
	pars, err := NewParser(lex)
	if err != nil {
		t.Error(err)
	}

	prog, err := pars.Program()
	if err != nil {
		t.Error(err)
	}

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 20, 3, 1, 2) {
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

	begin
		person.firstname := 1;

		writeln( person.firstname )
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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 2, 1, 0, 2) {
		return
	}
}
