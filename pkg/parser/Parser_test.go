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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 0, 0) {
		return
	}

	args := []ast.Expression{
		&ast.CharString{Token: token.NewToken(token.CharString, "Hello, World!"), Value: "Hello, World!"},
	}
	if !testProcedureStatement(t, prog.Block.Stats[0], "writeln", args) {
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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 1, 0) {
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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 3, 1, 0) {
		return
	}

	// first statement
	value := &ast.UIntegerLiteral{Token: token.Token{Text: "1", Kind: token.UIntLiteral}, Value: "1"}
	if !testAssignmentStatment(t, prog.Block.Stats[0], "a", value) {
		return
	}

	// second statement
	value = &ast.UIntegerLiteral{Token: token.Token{Text: "2", Kind: token.UIntLiteral}, Value: "2"}
	if !testAssignmentStatment(t, prog.Block.Stats[1], "b", value) {
		return
	}

	// third statement
	args := []ast.Expression{
		&ast.CharString{Token: token.NewToken(token.CharString, "Hello, world!"), Value: "Hello, world!"},
	}
	if !testProcedureStatement(t, prog.Block.Stats[2], "writeln", args) {
		return
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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 5, 1, 0) {
		return
	}

	expr := &ast.BinaryExpression{
		Operator: token.NewToken(token.Plus, "+"),
		Left:     &ast.Identifier{Token: token.NewToken(token.Identifier, "a"), Name: "a"},
		Right:    &ast.Identifier{Token: token.NewToken(token.Identifier, "b"), Name: "b"},
	}

	if !testAssignmentStatment(t, prog.Block.Stats[2], "sum", expr) {
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
	if !testAssignmentStatment(t, prog.Block.Stats[3], "a", uexpr) {
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

	function foo(n, m integer): integer;
	var result: integer;

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

	if !testProgramAST(t, prog, "MaxProgram", []string{}, 3, 0, 1) {
		return
	}

	params := []*ast.Parameter{
		{
			Names: []*ast.Identifier{
				{Token: token.NewToken(token.Identifier, "n"), Name: "n"},
				{Token: token.NewToken(token.Identifier, "m"), Name: "m"},
			},
			Type: dtype.NewInteger(token.NewToken(token.Integer, "integer")),
		},
	}
	if !testFuncDeclaration(t, prog.Block.Callables[0], "foo", "integer", params, 1, 1, 0) {
		return
	}

	funcDecl := prog.Block.Callables[0].(*ast.FuncDeclaration)
	if !testAssignmentStatment(
		t, funcDecl.Block.Stats[0], "foo", &ast.Identifier{Token: token.NewToken(token.Identifier, "2"), Name: "2"}) {
		return
	}
}

func TestParseProgramWithIfStatement(t *testing.T) {
	input := `
	program MaxProgram;

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

	if !testProgramAST(t, prog, "MaxProgram", []string{}, 3, 0, 1) {
		return
	}

	paramList := []*ast.Parameter{
		{
			Names: []*ast.Identifier{
				{Token: token.NewToken(token.Identifier, "n"), Name: "n"},
				{Token: token.NewToken(token.Identifier, "m"), Name: "m"},
			},
			Type: dtype.NewInteger(token.NewToken(token.Integer, "integer")),
		},
	}
	if !testFuncDeclaration(t, prog.Block.Callables[0], "max", "integer", paramList, 2, 1, 0) {
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
	if !testAssignmentStatment(t, ifStmt.TruePath, "result", value) {
		return
	}

	value.Token.Text = "m"
	value.Name = "m"

	if !testAssignmentStatment(t, ifStmt.ElsePath, "result", value) {
		return
	}
}

func TestParseProgramWithFunctionCall(t *testing.T) {
	input := `
	program MaxProgram;

	begin
		a := 100;
		b := 200;
		result := max(a, b);

		writeln(res)
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

	if !testProgramAST(t, prog, "MaxProgram", []string{}, 4, 0, 0) {
		return
	}

	value := &ast.FuncDesignator{
		Name: &ast.Identifier{Token: token.Token{Kind: token.Identifier, Text: "max"}, Name: "max"},
		Parameters: []ast.Expression{
			&ast.Identifier{Token: token.Token{Kind: token.Identifier, Text: "a"}, Name: "a"},
			&ast.Identifier{Token: token.Token{Kind: token.Identifier, Text: "b"}, Name: "b"},
		},
	}

	if !testAssignmentStatment(t, prog.Block.Stats[2], "result", value) {
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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 1, 2, 0) {
		return
	}

	names := [][]string{
		{"a", "b", "sum"},
		{"c", "d"},
	}
	if !testVarDeclaration(
		t, prog.Block.VarDeclaration, token.NewToken(token.Var, "var"), 2, names, []string{"integer", "integer"},
	) {
		return
	}
}

func TestParsingMultiplicationOperator(t *testing.T) {
	input := `
	program HelloWorld;
	var
		a, b, sum : integer;

	begin
		sum := a * (b * c);

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

	if !testProgramAST(t, prog, "HelloWorld", []string{}, 2, 1, 0) {
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

	if !testAssignmentStatment(t, prog.Block.Stats[0], "sum", value) {
		return
	}
}

func testAssignmentStatment(t *testing.T, stmt ast.Statement, variable string, value ast.Expression) bool {
	assignStmt, ok := stmt.(*ast.AssignStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.AssignStatement; found %v", assignStmt)
		return false
	}

	if assignStmt.Variable.Token.Kind != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.GetTokenName(token.Identifier), token.GetTokenName(assignStmt.Variable.Token.Kind))
		return false
	}

	if assignStmt.Variable.Name != variable {
		t.Errorf("expected variable to be %v, got %v instead,", variable, assignStmt.Variable.Name)
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
	t *testing.T, p *ast.ProgramAST, progName string, paramList []string, numStmts, numVarDefs, numCallables int,
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

	return testBlock(t, p.Block, numStmts, numVarDefs, numCallables)
}

func testBlock(t *testing.T, blk *ast.Block, numStmts, numVarDefs, numCallables int) bool {
	if len(blk.Stats) != numStmts {
		t.Errorf("expected %v statement(s) in block; found %v instead", numStmts, len(blk.Stats))
		return false
	}

	if blk.VarDeclaration != nil && len(blk.VarDeclaration.Decls) != numVarDefs {
		t.Errorf("expected %v variable declaration(s) in block; found %v instead", numVarDefs, len(blk.VarDeclaration.Decls))
		return false
	}

	if len(blk.Callables) != numCallables {
		t.Errorf("expected %v callable(s) in block; found %v instead", numCallables, len(blk.Callables))
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
	paramList []*ast.Parameter,
	numStmts, numVarDefs, numCallables int,
) bool {
	funcDecl, ok := fd.(*ast.FuncDeclaration)
	if !ok {
		t.Errorf("expected function declaration type, got %v", funcDecl)
		return false
	}

	if funcDecl.Token.Kind != token.Function {
		t.Errorf("function declaration has wrong token type, %v", funcDecl.Token.Text)
		return false
	}

	if funcDecl.Name.TokenLiteral() != funcName {
		t.Errorf("expected function name to be %v, got %v instead", funcName, funcDecl.Name.TokenLiteral())
		return false
	}

	if funcDecl.ReturnType.GetName() != retType {
		t.Errorf("expected return type to be %v, got %v instead", retType, funcDecl.ReturnType.GetName())
	}

	for i, j := 0, 0; i < len(paramList) && j < len(funcDecl.Parameters); i, j = i+1, j+1 {
		if paramList[i].Type.GetName() != funcDecl.Parameters[j].Type.GetName() {
			t.Errorf("expected parameter type to be %v, got %v instead",
				paramList[i].Type.GetName(), funcDecl.Parameters[j].Type.GetName())

			return false
		}

		for m, n := 0, 0; m < len(paramList[i].Names) && n < len(funcDecl.Parameters[j].Names); m, n = m+1, n+1 {
			if paramList[i].Names[m].Name != funcDecl.Parameters[j].Names[n].Name {
				t.Errorf("unmatched parameter names, %v and %v", paramList[i].Names[m], funcDecl.Parameters[j].Names[n])
				return false
			}
		}
	}

	if !testBlock(t, funcDecl.Block, numStmts, numVarDefs, numCallables) {
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
		t.Errorf("expected token type to be %v, got %v", token.GetTokenName(token.If), ifStmt.Token.Text)
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
	procStat, ok := stmt.(*ast.ProcedureStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.ProcedureStatement; found %v", procStat)
	}

	if procStat.ProcedureID.String() != procName {
		t.Errorf("expected procedure name %v, got %v instead", procName, procStat.ProcedureID.String())
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
			t.Errorf("expected variable type to be %v, got %v instead", varType, decl.Type.GetName())
			return false
		}
	}

	return true
}
