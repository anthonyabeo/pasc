package parser

import (
	"fmt"
	"reflect"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/dtype"
	"github.com/anthonyabeo/pasc/pkg/token"
)

// Parser performs syntactic analysis to validate the correctness of the input string.
//
// It implements an LL(1) Recursive-Descent Parser that relies on a stream of tokens
// from the lexer. The Parser checks and ensures that the input tokens stream conforms
// to the grammer of the language or returns an appropriate error otherwise.
type Parser struct {
	input     Lexer
	lookahead token.Token
}

// NewParser constructs and returns an instance of parser
func NewParser(lexer Lexer) (*Parser, error) {
	parser := Parser{input: lexer}
	if err := parser.consume(); err != nil {
		return nil, err
	}

	return &parser, nil
}

func (p *Parser) consume() error {
	token, err := p.input.NextToken()
	if err != nil {
		return err
	}

	p.lookahead = token

	return nil
}

// Match returns an error if the lookahead token does not match the expected
// type t, provided as an argument. If t is the token the parser expected,
// Match proceeds to the next token.
func (p *Parser) match(t token.Kind) error {
	if p.lookahead.Kind == t {
		if err := p.consume(); err != nil {
			return err
		}

		return nil
	}

	return fmt.Errorf("expecting %v; found %v", token.GetTokenName(t), p.lookahead.Text)
}

// Program represents the start symbol production rule in the grammer.
// It is the starting point in the parsing process.
func (p *Parser) Program() (*ast.ProgramAST, error) {
	var (
		err   error
		block *ast.Block
	)

	if err = p.programHeading(); err != nil {
		return nil, err
	}

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	if block, err = p.block(); err != nil {
		return nil, err
	}
	program := new(ast.ProgramAST)
	program.Block = block

	if err = p.match(token.Period); err != nil {
		return nil, err
	}

	return program, nil
}

func (p *Parser) programHeading() error {
	var err error

	if err = p.match(token.Program); err != nil {
		return err
	}

	if err = p.match(token.Identifier); err != nil {
		return err
	}

	// TODO: Implement matching optional program parameters

	return nil
}

func (p *Parser) block() (*ast.Block, error) {
	var (
		err          error
		decls        []*ast.VarDecl
		callables    []ast.Statement
		compoundStmt *ast.CompoundStatement
	)

	block := &ast.Block{}

	if decls, err = p.variableDeclarationPart(); err != nil {
		return nil, err
	}

	if callables, err = p.procedureAndFunctionDeclarationPart(); err != nil {
		return nil, err
	}

	if compoundStmt, err = p.compoundStatement(); err != nil {
		return nil, err
	}

	block.Vars = append(block.Vars, decls...)
	block.Stats = append(block.Stats, compoundStmt.Statements...)
	block.Callables = append(block.Callables, callables...)

	return block, nil
}

func (p *Parser) procedureAndFunctionDeclarationPart() ([]ast.Statement, error) {
	// procedure-and-function-declaration-part = { ( procedure-declaration | function-declaration ) ';' } .
	//
	// procedure-declaration := procedure-heading ';' directive
	// 						  | procedure-identication ';' procedure-block
	// 						  | procedure-heading ';' procedure-block .
	// procedure-heading := 'procedure' identier [ formal-parameter-list ] .
	// procedure-identication := 'procedure' procedure-identier .
	// procedure-identier := identier .
	// procedure-block := block .
	// directive := letter { letter | digit } .
	//
	// formal-parameter-list := '(' formal-parameter-section { ';' formal-parameter-section } ')' .
	// formal-parameter-section > value-parameter-specification
	//                          | variable-parameter-specification
	//                          | procedural-parameter-specification
	//                          | functional-parameter-specification .
	// formal-parameter-section > conformant-array-parameter-specification .
	//
	// value-parameter-specication = identifier-list ':' type-identifier .
	// variable-parameter-specification = 'var' identifier-list ':' type-identifier .
	// procedural-parameter-specification = procedure-heading .
	// functional-parameter-specification = function-heading .
	//
	// function-heading = 'function' identifier [ formal-parameter-list ] ':' result-type .
	// procedure-heading = 'procedure' identifier [ formal-parameter-list ] .
	//
	// result-type = simple-type-identifier | pointer-type-identifier .
	// simple-type-identifier = type-identifier .
	// pointer-type-identifier = type-identifier .
	//
	// type-identifier = identifier .
	//
	//
	// function-declaration := function-heading ';' directive
	//                      | function-identification ';' function-block
	//                      | function-heading ';' function-block .       <---- implementing this
	// function-identification = 'function' function-identifier .
	// function-identifier = identifier .
	// function-block = block .

	var callables []ast.Statement

	for p.lookahead.Kind == token.Function || p.lookahead.Kind == token.Procedure {
		switch p.lookahead.Kind {
		case token.Function:
			funcDecl, err := p.functionDeclaration()
			if err != nil {
				return nil, err
			}

			callables = append(callables, funcDecl)
		case token.Procedure:
		default:
			return nil, nil
		}
	}

	return callables, nil
}

func (p *Parser) functionHeading() (*ast.FuncDeclaration, error) {
	var (
		err       error
		paramList []*ast.Parameter
	)

	funcDecl := &ast.FuncDeclaration{Token: p.lookahead}

	if err = p.match(token.Function); err != nil {
		return nil, err
	}

	funcDecl.Name = ast.NewIdentifier(p.lookahead, p.lookahead.Text)

	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	paramList, err = p.formalParameterList()
	if err != nil {
		return nil, err
	}
	funcDecl.Parameters = append(funcDecl.Parameters, paramList...)

	if err = p.match(token.Colon); err != nil {
		return nil, err
	}

	//TODO: Fix this
	if !dtype.IsTypeIdentifier(p.lookahead.Kind) {
		return nil, fmt.Errorf("expected type identifier; got %v", p.lookahead.Text)
	}

	funcDecl.ReturnType = dtype.NewInteger(p.lookahead)
	if err = p.consume(); err != nil {
		return nil, err
	}

	return funcDecl, nil
}

func (p *Parser) functionDeclaration() (*ast.FuncDeclaration, error) {
	funcDecl, err := p.functionHeading()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	funcDecl.Block, err = p.block()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	return funcDecl, nil
}

func (p *Parser) formalParameterList() ([]*ast.Parameter, error) {
	// formal-parameter-list := '(' formal-parameter-section { ';' formal-parameter-section } ')' .
	var (
		err       error
		paramList []*ast.Parameter
	)

	if err = p.match(token.LParen); err != nil {
		return nil, err
	}

	for p.lookahead.Kind == token.Identifier || p.lookahead.Kind == token.Var ||
		p.lookahead.Kind == token.Procedure || p.lookahead.Kind == token.Function {

		switch p.lookahead.Kind {
		case token.Identifier:
			names, err := p.identifierList()
			if err != nil {
				return nil, err
			}
			params := &ast.Parameter{Names: names}

			if !dtype.IsTypeIdentifier(p.lookahead.Kind) {
				return nil, fmt.Errorf("expected type identifier; got %v", p.lookahead.Text)
			}

			// TODO: fix this hardcoding
			params.Type = dtype.NewInteger(p.lookahead)
			paramList = append(paramList, params)

			if err = p.consume(); err != nil {
				return nil, err
			}
		case token.Var:
		case token.Procedure:
		case token.Function:
		default:
			return nil, nil
		}

	}

	if err = p.match(token.RParen); err != nil {
		return nil, err
	}

	return paramList, nil
}

func (p *Parser) variableDeclarationPart() ([]*ast.VarDecl, error) {
	// TODO: extend to support potentially multiple variable declarations
	var (
		err     error
		varDecl *ast.VarDecl
		decls   []*ast.VarDecl
	)

	if p.lookahead.Kind != token.Var {
		return nil, nil
	}

	if err = p.consume(); err != nil {
		return nil, err
	}

	if varDecl, err = p.variableDeclaration(); err != nil {
		return nil, err
	}
	varDecl.Token = token.Token{Kind: token.Var, Text: "var"}
	decls = append(decls, varDecl)

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	return decls, nil
}

func (p *Parser) variableDeclaration() (*ast.VarDecl, error) {
	var (
		err   error
		names []*ast.Identifier
	)

	varDecl := new(ast.VarDecl)

	if names, err = p.identifierList(); err != nil {
		return nil, err
	}

	varDecl.Names = append(varDecl.Names, names...)

	if err = p.match(token.Colon); err != nil {
		return nil, err
	}

	if !dtype.IsTypeIdentifier(p.lookahead.Kind) {
		return nil, fmt.Errorf("expected type identifier; got %v", p.lookahead.Text)
	}

	// TODO: fix this hardcoding
	varDecl.Type = dtype.NewInteger(p.lookahead)

	if err = p.consume(); err != nil {
		return nil, err
	}

	return varDecl, nil
}

func (p *Parser) identifierList() ([]*ast.Identifier, error) {
	var (
		err   error
		names []*ast.Identifier
	)

	names = append(names, ast.NewIdentifier(p.lookahead, p.lookahead.Text))

	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	for p.lookahead.Kind == token.Comma || p.lookahead.Kind == token.Identifier {
		if err = p.match(token.Comma); err != nil {
			return nil, err
		}

		names = append(names, ast.NewIdentifier(p.lookahead, p.lookahead.Text))

		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}
	}

	return names, nil
}

func (p *Parser) compoundStatement() (*ast.CompoundStatement, error) {
	var (
		err     error
		stmtSeq []ast.Statement
	)

	if err = p.match(token.Begin); err != nil {
		return nil, err
	}

	if stmtSeq, err = p.statementSequence(); err != nil {
		return nil, err
	}

	if err = p.match(token.End); err != nil {
		return nil, err
	}

	return &ast.CompoundStatement{Statements: stmtSeq}, nil
}

func (p *Parser) statementSequence() ([]ast.Statement, error) {
	var (
		err     error
		stmt    ast.Statement
		stmtSeq []ast.Statement
	)

	for p.lookahead.Kind == token.Identifier || p.lookahead.Kind == token.Goto || p.lookahead.Kind == token.SemiColon ||
		p.lookahead.Kind == token.Begin || p.lookahead.Kind == token.With || p.lookahead.Kind == token.If ||
		p.lookahead.Kind == token.Case || p.lookahead.Kind == token.Repeat || p.lookahead.Kind == token.While ||
		p.lookahead.Kind == token.For {

		if stmt, err = p.statement(); err != nil {
			return nil, err
		}

		stmtSeq = append(stmtSeq, stmt)

		if p.lookahead.Kind == token.SemiColon {
			if err = p.consume(); err != nil {
				return nil, err
			}

			continue
		}
	}

	return stmtSeq, nil
}

func (p *Parser) statement() (ast.Statement, error) {
	var (
		err  error
		stmt ast.Statement
	)

	// structured-statement := compound-statement
	//                      | conditional-statement
	//                      | repetitive-statement
	//                      | with-statement .
	//
	// repetitive-statement := repeat-statement
	//						| while-statement
	//						| for-statement .
	// repeat-statement = 'repeat' statement-sequence 'until' Boolean-expression .
	// while-statement = 'while' Boolean-expression 'do' statement .
	// for-statement = 'for' control-variable ':=' initial-value ( 'to' | 'downto' ) fi nal-value 'do' statement .
	//
	// compound-statement := 'begin' statement-sequence 'end' .
	//
	// with-statement := 'with' record-variable-list 'do' statement .
	//
	// conditional-statement := if-statement
	//					     | case-statement .
	// if-statement = 'if' Boolean-expression 'then' statement [ else-part ] .
	// case-statement := 'case' case-index 'of' case-list-element { ';' case-list-element } [ ';' ] 'end' .

	// TODO: remove the if-else since it is already define in statementSequence.
	if p.lookahead.Kind == token.Begin || p.lookahead.Kind == token.With || p.lookahead.Kind == token.If ||
		p.lookahead.Kind == token.Case || p.lookahead.Kind == token.Repeat || p.lookahead.Kind == token.While ||
		p.lookahead.Kind == token.For {

		switch p.lookahead.Kind {
		case token.If:
			return p.ifStatement()
		case token.Begin:
			return p.compoundStatement()
		case token.While:
		case token.With:
		case token.Case:
		case token.Repeat:
		case token.For:
		default:

		}
		// TODO Ignoring the optional Label for now.
		// TODO the lookahead set of the statement include the the first and follow sets of SimpleStatement and ProcedureStament. These tokens include GOTO, procedure_identifier, etc. The current implementation only checks for the procedure identifier.
	} else if p.lookahead.Kind == token.Identifier || p.lookahead.Kind == token.Goto {
		if stmt, err = p.simpleStatement(); err != nil {
			return nil, err
		}
	}

	return stmt, nil
}

func (p *Parser) simpleStatement() (ast.Statement, error) {
	// TODO: get rid of this simpleStatement and implement the individual methods in the switch case in statement
	var (
		err  error
		stmt ast.Statement
	)

	switch p.lookahead.Kind {
	case token.Identifier:
		// ident is either a procedure identifier or variable identifier
		ident := p.lookahead

		if err := p.consume(); err != nil {
			return nil, err
		}

		if p.lookahead.Kind == token.LParen {
			if stmt, err = p.procedureStatement(ident); err != nil {
				return nil, err
			}
		} else if p.lookahead.Kind == token.Initialize {
			if stmt, err = p.assignmentStatement(ident); err != nil {
				return nil, err
			}
		}

	default:
		return nil, fmt.Errorf("expecting procedure_name, goto or assignment; found %v", p.lookahead)
	}

	return stmt, nil
}

func (p *Parser) procedureStatement(tt token.Token) (*ast.ProcedureStatement, error) {
	// TODO: complete implementation
	// procedure-statement = procedure-identitier ( [ actual-parameter-list ]
	//                                | read-parameter-list
	//                                | readln-parameter-list
	//                                | write-parameter-list
	//								  | writeln-parameter-list ) .
	var err error

	ps := ast.NewProcedureStatement(ast.NewIdentifier(tt, tt.Text))

	actualParamList, err := p.actualParameterList()
	if err != nil {
		return nil, err
	}

	writelnParamList, err := p.writelnParameterList()
	if err != nil {
		return nil, err
	}

	ps.ParamList = append(ps.ParamList, actualParamList...)
	ps.ParamList = append(ps.ParamList, writelnParamList...)

	return ps, nil
}

func (p *Parser) assignmentStatement(tt token.Token) (*ast.AssignStatement, error) {
	var err error

	as := ast.NewAssignmentStatement(ast.NewIdentifier(tt, tt.Text))

	if err = p.match(token.Initialize); err != nil {
		return nil, err
	}

	as.Value, err = p.expression()
	if err != nil {
		return nil, err
	}

	return as, nil
}

func (p *Parser) expression() (ast.Expression, error) {
	simpleExpr, err := p.simpleExpression()
	if err != nil {
		return nil, err
	}

	if p.isRelationalOp() {
		relExpr := &ast.BinaryExpression{Operator: p.lookahead, Left: simpleExpr}
		if err := p.consume(); err != nil {
			return nil, err
		}

		relExpr.Right, err = p.simpleExpression()
		if err != nil {
			return nil, err
		}

		return relExpr, nil
	}

	return simpleExpr, nil
}

func (p *Parser) simpleExpression() (ast.Expression, error) {
	var (
		err  error
		sign token.Token
		term ast.Expression
	)

	if p.isSign() {
		sign = p.lookahead
		if err = p.consume(); err != nil {
			return nil, err
		}
	}

	term, err = p.term()
	if err != nil {
		return nil, err
	}

	if p.isAddingOp() {
		binExp := &ast.BinaryExpression{Left: term, Operator: p.lookahead}
		if err = p.consume(); err != nil {
			return nil, err
		}

		binExp.Right, err = p.term()
		if err != nil {
			return nil, err
		}

		return binExp, nil
	}

	if !reflect.DeepEqual(sign, token.Token{}) {
		unaryExp := &ast.UnaryExpression{Operator: sign, Operand: term}
		return unaryExp, nil
	}

	return term, nil
}

func (p *Parser) term() (ast.Expression, error) {
	// TODO: implement {multiplying-operator factor}
	fact, err := p.factor()
	if err != nil {
		return nil, err
	}

	return fact, err
}

func (p *Parser) factor() (ast.Expression, error) {
	// TODO: implements only 'bounded-identifier' alternative
	// factor > bound-identifier
	// factor > variable-access
	//		  | unsigned-constant
	//		  | function-designator
	//		  | set-constructor
	//		  | '(' expression ')'
	//		  | 'not' factor

	switch p.lookahead.Kind {
	case token.Identifier:
		tt := p.lookahead
		if err := p.consume(); err != nil {
			return nil, err
		}

		if p.lookahead.Kind == token.LParen {
			return p.functionDesignator(tt)
		}

		return p.variableAccess(tt)
	case token.UIntLiteral, token.URealLiteral, token.CharString, token.Nil, token.ConstIdentifier:
		return p.unsignedConstant()
	case token.LParen:
		if err := p.match(token.LParen); err != nil {
			return nil, err
		}

		expr, err := p.expression()
		if err != nil {
			return nil, err
		}

		if err = p.match(token.RParen); err != nil {
			return nil, err
		}

		return expr, nil
	default:
		return nil, fmt.Errorf("expected identifier or integer, got %v", p.lookahead.Text)
	}
}

func (p *Parser) variableAccess(t token.Token) (ast.Expression, error) {
	// if err := p.consume(); err != nil {
	// 	return nil, err
	// }

	return ast.NewIdentifier(t, t.Text), nil
}

func (p *Parser) unsignedConstant() (ast.Expression, error) {
	// TODO: complete implementation
	// unsigned-constant := unsigned-number
	//					 | character-string
	//					 | constant-identier
	//					 | 'nil' .
	// constant-identi er = identifier .

	tt := p.lookahead

	if err := p.consume(); err != nil {
		return nil, err
	}

	switch tt.Kind {
	case token.UIntLiteral, token.URealLiteral:
		return p.unsignedNumber(tt)
	case token.CharString:
		return &ast.CharString{Token: tt, Value: tt.Text}, nil
	case token.ConstIdentifier:
		return ast.NewIdentifier(tt, tt.Text), nil
	case token.Nil:
		return &ast.NilValue{Token: tt}, nil
	default:
		return nil, fmt.Errorf("expected unsigned constant type, instead got, %v", tt.Text)
	}
}

func (p *Parser) unsignedNumber(tt token.Token) (ast.Expression, error) {
	// unsigned-number := unsigned-integer
	//                 | unsigned-real .
	// unsigned-integer := digit-sequence .
	// unsigned-real := digit-sequence '.' fractional-part [ 'e' scale-factor ]
	//                | digit-sequence 'e' scale-factor .

	switch tt.Kind {
	case token.UIntLiteral:
		return ast.NewUIntegerLiteral(tt), nil
	case token.URealLiteral:
		// TODO: implement
		// return ast.NewURealLiteral()
		return nil, nil
	default:
		return nil, fmt.Errorf("expected unsigned real or integer, instead got, %v", p.lookahead.Text)
	}
}

func (p *Parser) isMultiplyOp() bool {
	return p.lookahead.Kind == token.Star ||
		p.lookahead.Kind == token.FwdSlash ||
		p.lookahead.Kind == token.Div ||
		p.lookahead.Kind == token.Mod ||
		p.lookahead.Kind == token.And
}

func (p *Parser) isAddingOp() bool {
	return p.lookahead.Kind == token.Plus ||
		p.lookahead.Kind == token.Minus ||
		p.lookahead.Kind == token.Or
}

func (p *Parser) isRelationalOp() bool {
	return p.lookahead.Kind == token.Equal ||
		p.lookahead.Kind == token.LessThanGreaterThan ||
		p.lookahead.Kind == token.LessThan ||
		p.lookahead.Kind == token.GreaterThan ||
		p.lookahead.Kind == token.LessThanOrEqual ||
		p.lookahead.Kind == token.GreaterThanOrEqual ||
		p.lookahead.Kind == token.In

}

func (p *Parser) isSign() bool {
	return p.lookahead.Kind == token.Plus ||
		p.lookahead.Kind == token.Minus
}

func (p *Parser) ifStatement() (*ast.IfStatement, error) {
	// if-statement := 'if' Boolean-expression 'then' statement [ else-part ] .
	// Boolean-expression := expression .
	// else-part := 'else' statement .
	var err error

	ifStmt := &ast.IfStatement{Token: p.lookahead}

	if err = p.match(token.If); err != nil {
		return nil, err
	}

	ifStmt.BoolExpr, err = p.expression()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Then); err != nil {
		return nil, err
	}

	ifStmt.TruePath, err = p.statement()
	if err != nil {
		return nil, err
	}

	if p.lookahead.Kind == token.Else {
		ifStmt.ElsePath, err = p.elsePart()
		if err != nil {
			return nil, err
		}
	}

	return ifStmt, nil
}

func (p *Parser) elsePart() (ast.Statement, error) {
	if err := p.match(token.Else); err != nil {
		return nil, err
	}

	stmt, err := p.statement()
	if err != nil {
		return nil, err
	}

	return stmt, nil
}

func (p *Parser) functionDesignator(t token.Token) (*ast.FuncDesignator, error) {
	// function-designator = function-identitier [ actual-parameter-list ] .
	// function-identitier = identitier .
	var err error

	funcCall := &ast.FuncDesignator{Name: ast.NewIdentifier(t, t.Text)}
	funcCall.Parameters, err = p.actualParameterList()
	if err != nil {
		return nil, err
	}

	return funcCall, nil
}

func (p *Parser) actualParameterList() ([]ast.Expression, error) {
	// actual-parameter-list = '(' actual-parameter { ',' actual-parameter } ')' .
	// actual-parameter := expression
	//			         | variable-access
	//					 | procedure-identitier
	//	                 | function-identitier .
	var (
		err       error
		param     ast.Expression
		paramList []ast.Expression
	)

	if err = p.match(token.LParen); err != nil {
		return nil, err
	}

	param, err = p.actualParameter()
	if err != nil {
		return nil, err
	}
	paramList = append(paramList, param)

	for p.lookahead.Kind == token.Comma {
		if err := p.match(token.Comma); err != nil {
			return nil, err
		}

		param, err = p.actualParameter()
		if err != nil {
			return nil, err
		}
		paramList = append(paramList, param)
	}

	if err = p.match(token.RParen); err != nil {
		return nil, err
	}

	return paramList, nil
}

func (p *Parser) actualParameter() (ast.Expression, error) {
	expr, err := p.expression()
	if err != nil {
		return nil, err
	}

	return expr, nil
}

func (p *Parser) writelnParameterList() ([]ast.Expression, error) {
	// writeln-parameter-list = [ '(' (  file-variable | write-parameter ) { ',' write-parameter } ')' ] .
	var (
		err              error
		writeParam       *ast.WriteParameter
		writelnParamList []ast.Expression
	)

	if p.lookahead.Kind == token.LParen {
		if err = p.match(token.LParen); err != nil {
			return nil, err
		}

		// TODO: implement file-variable alternative
		writeParam, err = p.writeParameter()
		if err != nil {
			return nil, err
		}
		writelnParamList = append(writelnParamList, writeParam)

		for p.lookahead.Kind != token.Comma {
			if err = p.match(token.Comma); err != nil {
				return nil, err
			}

			writeParam, err = p.writeParameter()
			if err != nil {
				return nil, err
			}
			writelnParamList = append(writelnParamList, writeParam)
		}
	}

	return writelnParamList, nil
}

func (p *Parser) writeParameter() (*ast.WriteParameter, error) {
	// write-parameter := expression [ ':' expression [ ':' expression ] ] .
	var (
		err  error
		expr ast.Expression
	)

	expr, err = p.expression()
	if err != nil {
		return nil, err
	}

	wp := &ast.WriteParameter{E: expr}

	if p.lookahead.Kind == token.Colon {
		if err = p.match(token.Colon); err != nil {
			return nil, err
		}

		wp.TotalWidth, err = p.expression()
		if err != nil {
			return nil, err
		}

		if p.lookahead.Kind == token.Colon {
			if err = p.match(token.Colon); err != nil {
				return nil, err
			}

			wp.FracDigits, err = p.expression()
			if err != nil {
				return nil, err
			}
		}
	}

	return wp, nil
}
