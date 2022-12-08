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

	if !testProgramAST(t, prog, 1, 0, 0) {
		return
	}

	procStat, ok := prog.Block.Stats[0].(*ast.ProcedureStatement)
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

	if !testProgramAST(t, prog, 1, 1, 0) {
		return
	}

	varDecl := prog.Block.Vars[0]
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

	if !testProgramAST(t, prog, 3, 1, 0) {
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
	procStat, ok := prog.Block.Stats[2].(*ast.ProcedureStatement)
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

	if !testProgramAST(t, prog, 5, 1, 0) {
		return
	}

	stmt, ok := prog.Block.Stats[2].(*ast.AssignStatement)
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

	if lhs.Name != "a" {
		t.Errorf("expected LHS identifier to be 'a', got %v", lhs.Name)
	}

	// Unary Statement
	stmt, ok = prog.Block.Stats[3].(*ast.AssignStatement)
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

	intLit, ok := uexpr.Operand.(*ast.UIntegerLiteral)
	if !ok {
		t.Errorf("expected operand to be integer literal, got %v", intLit)
	}

	if intLit.Value != "5" {
		t.Errorf("expected integer value to be 5, got %v", intLit.Value)
	}
}

func TestParseProgramWithFunctionDeclaration(t *testing.T) {
	input := `
	program MaxProgram;

	function foo(n, m integer): integer;
	var result: integer;

	begin
		foo := 2;
	end;

	begin
		a := 100;
		b := 200;

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

	if !testProgramAST(t, prog, 3, 0, 1) {
		return
	}

	funcDecl, ok := prog.Block.Callables[0].(*ast.FuncDeclaration)
	if !ok {
		t.Errorf("expected function declaration type, got %v", funcDecl)
	}

	if funcDecl.Token.Kind != token.Function {
		t.Errorf("function declaration has wrong token type, %v", funcDecl.Token.Text)
	}

	if funcDecl.Name.Token.Kind != token.Identifier {
		t.Errorf("function name is not of type identifier. It is %v", funcDecl.Name.Token.Text)
	}

	if funcDecl.Name.Name != "foo" {
		t.Errorf("expected function name to be 'foo', got %v", funcDecl.Name.Name)
	}

	if len(funcDecl.Parameters) != 1 {
		t.Errorf("more than 1 set of parameters")
	}

	params := funcDecl.Parameters[0]
	if len(params.Names) != 2 {
		t.Errorf("expected 2 parameters, got %v", len(params.Names))
	}

	intType, ok := params.Type.(*dtype.Integer)
	if !ok || intType.Token.Kind != token.Integer {
		t.Errorf("parameters are not of %v type", params.Type.GetName())
	}

	intType, ok = funcDecl.ReturnType.(*dtype.Integer)
	if !ok || intType.Token.Kind != token.Integer {
		t.Errorf("return type is not of %v type", params.Type.GetName())
	}

	if len(funcDecl.Block.Stats) != 1 {
		t.Errorf("expected function block to contain 1 statement, found %v", len(funcDecl.Block.Stats))
	}

	assignStmt, ok := funcDecl.Block.Stats[0].(*ast.AssignStatement)
	if !ok {
		t.Errorf("expected statement of type, ast.AssignStatement; found %v", assignStmt)
	}

	if assignStmt.Variable.Token.Kind != token.Identifier {
		t.Errorf("expected variable to be of kind %v, got %v",
			token.GetTokenName(token.Identifier), token.GetTokenName(assignStmt.Variable.Token.Kind))
	}

	intLit, ok := assignStmt.Value.(*ast.UIntegerLiteral)
	if !ok || intLit.Token.Kind != token.UIntLiteral {
		t.Errorf("expected value of assignment type to be integer literal")
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
			result := m

		max := result
	end;

	begin
		a := 100;
		b := 200;

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

	if !testProgramAST(t, prog, 3, 0, 1) {
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

	if !testProgramAST(t, prog, 4, 0, 0) {
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

func testProgramAST(t *testing.T, p *ast.ProgramAST, numStmts, numVarDefs, numCallables int) bool {
	if p == nil {
		t.Error("AST not created")
		return false
	}

	return testBlock(t, p.Block, numStmts, numVarDefs, numCallables)
}

func testBlock(t *testing.T, blk *ast.Block, numStmts, numVarDefs, numCallables int) bool {
	if len(blk.Stats) != numStmts {
		t.Errorf("expected %v statement(s) in block; found %v instead", numStmts, len(blk.Stats))
		return false
	}

	if len(blk.Vars) != numVarDefs {
		t.Errorf("expected %v variable declaration(s) in block; found %v instead", numVarDefs, len(blk.Vars))
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
