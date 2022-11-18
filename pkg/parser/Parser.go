package parser

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/ast"
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
func (p *Parser) match(t token.Type) error {
	if p.lookahead.Type == t {
		p.consume()

		return nil
	}

	return fmt.Errorf("expecting %v; found %v", token.GetTokenName(t), p.lookahead.Text)
}

// Program represents the start symbol production rule in the grammer.
// It is the starting point in the parsing process.
func (p *Parser) Program() (*ast.ProgramAST, error) {
	var (
		err     error
		stmtSeq []ast.Statement
	)

	if err = p.programHeading(); err != nil {
		return nil, err
	}

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	if stmtSeq, err = p.block(); err != nil {
		return nil, err
	}
	program := new(ast.ProgramAST)
	program.Stats = append(program.Stats, stmtSeq...)

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

func (p *Parser) block() ([]ast.Statement, error) {
	var (
		err     error
		stmtSeq []ast.Statement
	)

	if stmtSeq, err = p.compoundStatement(); err != nil {
		return nil, err
	}

	return stmtSeq, nil
}

func (p *Parser) compoundStatement() ([]ast.Statement, error) {
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

	return stmtSeq, nil
}

func (p *Parser) statementSequence() ([]ast.Statement, error) {
	var (
		err     error
		stmt    ast.Statement
		stmtSeq []ast.Statement
	)

	// TODO Update to support 1+ statements
	if stmt, err = p.statement(); err != nil {
		return nil, err
	}

	stmtSeq = append(stmtSeq, stmt)

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	return stmtSeq, nil
}

func (p *Parser) statement() (ast.Statement, error) {
	var (
		err  error
		stmt ast.Statement
	)

	// TODO Ignoring the optional Label for now.
	// TODO the lookahead set of the statement include the the first and follow sets of SimpleStatement and ProcedureStament. These tokens include GOTO, procedure_identifier, etc. The current implementation only checks for the procedure identifier.
	if p.lookahead.Type == token.Identifier { // || GOTO || procedure_identifier
		if stmt, err = p.simpleStatement(); err != nil {
			return nil, err
		}
	} else {
		// TODO this branch handles the StructuredStatement alternative path
	}

	return stmt, nil
}

func (p *Parser) simpleStatement() (ast.Statement, error) {
	var (
		err error
		ps  *ast.ProcedureStatement
	)

	switch p.lookahead.Type {
	case token.Identifier:
		if ps, err = p.procedureStatement(); err != nil {
			return nil, err
		}
	default:
		return nil, fmt.Errorf("expecting procedure_name, goto or assignment; found %v", p.lookahead)
	}

	return ps, nil
}

func (p *Parser) procedureStatement() (*ast.ProcedureStatement, error) {
	var err error

	ps := ast.NewProcedureStatement(ast.NewIdentifier(p.lookahead, p.lookahead.Text))

	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	if err = p.match(token.LParen); err != nil {
		return nil, err
	}

	// TODO: compute the parameter list
	ps.ParamList = append(ps.ParamList, ast.NewStringLiteral(p.lookahead, p.lookahead.Text))

	// TODO: check for procedure parameter list here.
	// TODO: For now we just match the string literal
	if err = p.match(token.StrLiteral); err != nil {
		return nil, err
	}

	if err = p.match(token.RParen); err != nil {
		return nil, err
	}

	return ps, nil
}
