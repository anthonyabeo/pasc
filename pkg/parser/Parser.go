package parser

import (
	"fmt"
	"reflect"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/symbols"
	"github.com/anthonyabeo/pasc/pkg/token"
	"github.com/anthonyabeo/pasc/pkg/types"
	"github.com/anthonyabeo/pasc/pkg/types/structured"
)

// Parser performs syntactic analysis to validate the correctness of the input string.
//
// It implements an LL(3) Recursive-Descent Parser that relies on a stream of tokens
// from the lexer. The Parser checks and ensures that the input tokens stream conforms
// to the grammar of the language or returns an appropriate error otherwise.
type Parser struct {
	input     Lexer
	lookahead [3]token.Token
	k, idx    int
	curScope  symbols.Scope
	symTable  *symbols.GlobalScope
}

// NewParser constructs and returns an instance of parser
func NewParser(lexer Lexer) (*Parser, error) {
	parser := Parser{input: lexer, k: 3, idx: 0}
	for i := 0; i < parser.k; i++ {
		if err := parser.consume(); err != nil {
			return nil, err
		}
	}

	globalScope := symbols.NewGlobalScope(nil)
	parser.symTable = globalScope
	parser.curScope = globalScope

	return &parser, nil
}

func (p *Parser) consume() error {
	t, err := p.input.NextToken()
	if err != nil {
		return err
	}

	p.lookahead[p.idx] = t
	p.idx = (p.idx + 1) % p.k

	return nil
}

func (p *Parser) lAheadToken(i int) token.Token {
	return p.lookahead[(p.idx+i-1)%p.k]
}

func (p *Parser) lAheadKind(i int) token.Kind {
	return p.lAheadToken(i).Kind
}

// SymbolTable returns the scope tree constructed during parsing
func (p *Parser) SymbolTable() *symbols.GlobalScope {
	return p.symTable
}

// Match returns an error if the lookahead token does not match the expected
// type t, provided as an argument. If t is the token the parser expected,
// Match proceeds to the next token.
func (p *Parser) match(t token.Kind) error {
	if p.lAheadKind(1) == t {
		if err := p.consume(); err != nil {
			return err
		}

		return nil
	}

	return fmt.Errorf("expecting %v; found %v", t, p.lAheadToken(1).Text)
}

// Program represents the start symbol production rule in the grammar.
// It is the starting point in the parsing process.
//
// The grammar production rules for Program is as follows:
//
// program = program-heading ';' program-block '.' .
// program-block = block .
func (p *Parser) Program() (*ast.Program, error) {
	var (
		err           error
		programName   *ast.Identifier
		programParams []*ast.Identifier
	)

	if programName, programParams, err = p.programHeading(); err != nil {
		return nil, err
	}

	program := &ast.Program{
		Name:      programName,
		ParamList: programParams,
		Token:     token.Token{Kind: token.Program, Text: "program"}}

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	program.Block, err = p.block()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Period); err != nil {
		return nil, err
	}

	return program, nil
}

// program-heading = 'program' identifier [ '(' program-parameter-list ')' ] .
func (p *Parser) programHeading() (*ast.Identifier, []*ast.Identifier, error) {
	var (
		err           error
		programName   *ast.Identifier
		programParams []*ast.Identifier
	)

	if err = p.match(token.Program); err != nil {
		return nil, nil, err
	}

	programName = &ast.Identifier{
		Token: token.NewToken(token.Identifier, p.lAheadToken(1).Text),
		Name:  p.lAheadToken(1).Text,
	}

	if err = p.match(token.Identifier); err != nil {
		return nil, nil, err
	}

	if p.lAheadKind(1) == token.LParen {
		if err = p.match(token.LParen); err != nil {
			return nil, nil, err
		}

		if programParams, err = p.programParameterList(); err != nil {
			return nil, nil, err
		}

		if err := p.match(token.RParen); err != nil {
			return nil, nil, err
		}
	}

	return programName, programParams, nil
}

// program-parameter-list = identifier-list .
func (p *Parser) programParameterList() ([]*ast.Identifier, error) {
	return p.identifierList()
}

func (p *Parser) block() (*ast.Block, error) {
	var (
		err             error
		varDecl         *ast.VarDeclaration
		callables       []ast.Statement
		constDefinition *ast.ConstDefinition
		labelDefinition *ast.LabelDefinition
		typeDefinition  *ast.TypeDefinition
		compoundStmt    *ast.CompoundStatement
	)

	block := &ast.Block{}

	for p.isBlockComponent() {
		switch p.lAheadKind(1) {
		case token.Label:
			labelDefinition, err = p.labelDefinitionPart()
			if err != nil {
				return nil, err
			}
		case token.Type:
			typeDefinition, err = p.typeDefinitionPart()
			if err != nil {
				return nil, err
			}
		case token.Const:
			constDefinition, err = p.constDefinitionPart()
			if err != nil {
				return nil, err
			}
		case token.Var:
			varDecl, err = p.variableDeclarationPart()
			if err != nil {
				return nil, err
			}
		case token.Procedure, token.Function:
			callables, err = p.procedureAndFunctionDeclarationPart()
			if err != nil {
				return nil, err
			}
		case token.Begin:
			compoundStmt, err = p.compoundStatement()
			if err != nil {
				return nil, err
			}
		}
	}

	block.Types = typeDefinition
	block.VarDeclaration = varDecl
	block.Consts = constDefinition
	block.Labels = labelDefinition
	block.Stats = append(block.Stats, compoundStmt.Statements...)
	block.Callables = append(block.Callables, callables...)

	return block, nil
}

func (p *Parser) labelDefinitionPart() (*ast.LabelDefinition, error) {
	labelDefinition := &ast.LabelDefinition{Token: p.lAheadToken(1)}
	if err := p.match(token.Label); err != nil {
		return nil, err
	}

	label := &ast.UIntegerLiteral{Token: p.lAheadToken(1), Value: p.lAheadToken(1).Text}
	if err := p.match(token.UIntLiteral); err != nil {
		return nil, err
	}
	labelDefinition.Labels = append(labelDefinition.Labels, label)

	for p.lAheadKind(1) == token.Comma {
		if err := p.consume(); err != nil {
			return nil, err
		}

		label := &ast.UIntegerLiteral{Token: p.lAheadToken(1), Value: p.lAheadToken(1).Text}
		if err := p.match(token.UIntLiteral); err != nil {
			return nil, err
		}
		labelDefinition.Labels = append(labelDefinition.Labels, label)
	}

	if err := p.match(token.SemiColon); err != nil {
		return nil, err
	}

	return labelDefinition, nil
}

func (p *Parser) isBlockComponent() bool {
	return p.lAheadKind(1) == token.Type ||
		p.lAheadKind(1) == token.Var ||
		p.lAheadKind(1) == token.Procedure ||
		p.lAheadKind(1) == token.Function ||
		p.lAheadKind(1) == token.Const ||
		p.lAheadKind(1) == token.Begin
}

// type-definition-part = [ 'type' type-definition ';' { type-definition ';' } ] .
func (p *Parser) typeDefinitionPart() (*ast.TypeDefinition, error) {
	var (
		err     error
		typeDef *ast.TypeDef
	)

	typeDefinition := &ast.TypeDefinition{Token: p.lAheadToken(1)}
	if err = p.match(token.Type); err != nil {
		return nil, err
	}

	typeDef, err = p.typeDefinition()
	if err != nil {
		return nil, err
	}
	typeDefinition.Types = append(typeDefinition.Types, typeDef)

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	for p.lAheadKind(1) == token.Identifier {
		typeDef, err = p.typeDefinition()
		if err != nil {
			return nil, err
		}
		typeDefinition.Types = append(typeDefinition.Types, typeDef)

		if err = p.match(token.SemiColon); err != nil {
			return nil, err
		}
	}

	return typeDefinition, nil
}

// type-definition = identifier '=' type-denoter .
func (p *Parser) typeDefinition() (*ast.TypeDef, error) {
	var (
		err     error
		typeDef *ast.TypeDef
	)

	typeDef = &ast.TypeDef{Name: &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text}}
	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	if err = p.match(token.Equal); err != nil {
		return nil, err
	}

	typeDef.TypeDenoter, err = p.typeDenoter()
	if err != nil {
		return nil, err
	}

	p.curScope.Define(
		symbols.NewTypeDefSymbol(typeDef.Name.Name, symbols.TYPE, typeDef.TypeDenoter))

	return typeDef, nil
}

func (p *Parser) typeIdentifier() (types.Type, error) {
	var (
		err error
		typ types.Type
	)

	sym := p.curScope.Resolve(p.lAheadToken(1).Text)
	if sym == nil {
		return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
	} else if sym.GetKind() != symbols.TYPE {
		return nil, fmt.Errorf("symbol %v is not an appropriate data type", p.lAheadToken(1).Text)
	} else {
		if err = p.consume(); err != nil {
			return nil, err
		}

		typ = sym.GetType()
	}

	return typ, nil
}

// type-denoter = type-identifier | new-type .
// type-identifier = identifier .
func (p *Parser) typeDenoter() (types.Type, error) {
	var (
		err error
		typ types.Type
	)

	if p.lAheadKind(1) == token.Identifier || p.lAheadKind(1) == token.Integer ||
		p.lAheadKind(1) == token.Boolean || p.lAheadKind(1) == token.Char || p.lAheadKind(1) == token.Real {

		typ, err = p.typeIdentifier()
		if err != nil {
			return nil, err
		}
	} else {
		if p.isNewType() {
			switch p.lAheadKind(1) {
			case token.Array:
				typ, err = p.arrayType()
				if err != nil {
					return nil, err
				}
			case token.LParen:
				typ, err = p.enumType()
				if err != nil {
					return nil, err
				}
			case token.Record:
				typ, err = p.recordType()
				if err != nil {
					return nil, err
				}
			case token.File:

			case token.Set:
				typ, err = p.setType()
				if err != nil {
					return nil, err
				}
			case token.Plus, token.Minus, token.UIntLiteral, token.CharString, token.URealLiteral, token.Identifier:
				typ, err = p.subRangeType()
				if err != nil {
					return nil, err
				}
			case token.Packed:
			}
		}
	}

	return typ, nil
}

// record-type = 'record' field-list 'end' .
func (p *Parser) recordType() (*structured.Record, error) {
	var err error

	record := &structured.Record{Token: p.lAheadToken(1)}
	if err = p.match(token.Record); err != nil {
		return nil, err
	}

	record.FieldList, err = p.fieldList()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.End); err != nil {
		return nil, err
	}

	return record, nil
}

// field-list = [ ( fixed-part [ ';' variant-part ] | variant-part ) [ ';' ] ] .
func (p *Parser) fieldList() ([]structured.Field, error) {
	var (
		err       error
		varPart   *structured.VariantPart
		fieldList []structured.Field
	)

	if p.lAheadKind(1) == token.Case {
		varPart, err = p.variantPart()
		if err != nil {
			return nil, err
		}
		fieldList = append(fieldList, varPart)
	} else {
		fixedPart := &structured.FixedPart{}

		recordSec, err := p.recordSection()
		if err != nil {
			return nil, err
		}
		fixedPart.Entry = append(fixedPart.Entry, recordSec)
		fieldList = append(fieldList, fixedPart)

		for p.lAheadKind(1) == token.SemiColon {
			if err = p.consume(); err != nil {
				return nil, err
			}

			if p.lAheadKind(1) == token.Case {
				varPart, err = p.variantPart()
				if err != nil {
					return nil, err
				}
				fieldList = append(fieldList, varPart)
			} else {
				recordSec, err := p.recordSection()
				if err != nil {
					return nil, err
				}
				fixedPart.Entry = append(fixedPart.Entry, recordSec)
				fieldList = append(fieldList, fixedPart)
			}
		}
	}

	if p.lAheadKind(1) == token.SemiColon {
		if err = p.consume(); err != nil {
			return nil, err
		}
	}

	return fieldList, nil
}

// record-section = identifier-list ':' type-denoter .
func (p *Parser) recordSection() (*structured.RecordSection, error) {
	var err error

	recordSec := &structured.RecordSection{}
	recordSec.List, err = p.identifierList()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Colon); err != nil {
		return nil, err
	}

	recordSec.Type, err = p.typeDenoter()
	if err != nil {
		return nil, err
	}

	return recordSec, nil
}

// variant-part = 'case' variant-selector 'of' variant { ';' variant } .
func (p *Parser) variantPart() (*structured.VariantPart, error) {
	var err error

	variantPart := &structured.VariantPart{Token: p.lAheadToken(1)}
	if err = p.match(token.Case); err != nil {
		return nil, err
	}

	variantPart.VariantSelector, err = p.variantSelector()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Of); err != nil {
		return nil, err
	}

	variant, err := p.variant()
	if err != nil {
		return nil, err
	}
	variantPart.Variants = append(variantPart.Variants, variant)

	for p.lAheadKind(1) == token.SemiColon {
		if err = p.consume(); err != nil {
			return nil, err
		}

		variant, err := p.variant()
		if err != nil {
			return nil, err
		}
		variantPart.Variants = append(variantPart.Variants, variant)
	}

	return variantPart, nil
}

// variant-selector = [ tag-field ':' ] tag-type .
// tag-field = identifier .
// tag-type = ordinal-type-identifier .
func (p *Parser) variantSelector() (*structured.VariantSelector, error) {
	var err error

	selector := &structured.VariantSelector{}
	if p.lAheadKind(1) == token.Identifier {
		selector.TagField = &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text}
		if err = p.consume(); err != nil {
			return nil, err
		}

		if err = p.match(token.Colon); err != nil {
			return nil, err
		}
	}

	sym := p.curScope.Resolve(p.lAheadToken(1).Text)
	if sym == nil {
		return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
	} else if sym.GetKind() != symbols.TYPE {
		return nil, fmt.Errorf("symbol %v is not an appropriate data type", p.lAheadToken(1).Text)
	} else {
		typ, ok := sym.GetType().(types.Ordinal)
		if !ok {
			return nil, fmt.Errorf(
				"%v is not an ordinal type. Must be one of integer, Boolean, char, enum, subrange",
				p.lAheadToken(1).Text)
		}

		selector.TagType = typ

		if err = p.consume(); err != nil {
			return nil, err
		}
	}

	return selector, nil
}

// variant = case-constant-list ':' '(' field-list ')' .
func (p *Parser) variant() (*structured.Variant, error) {
	caseConstList, err := p.caseConstantList()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Colon); err != nil {
		return nil, err
	}

	if err = p.match(token.LParen); err != nil {
		return nil, err
	}

	fieldList, err := p.fieldList()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.RParen); err != nil {
		return nil, err
	}

	return &structured.Variant{CaseConstList: caseConstList, FieldList: fieldList}, nil
}

func (p *Parser) subRangeType() (*structured.SubRange, error) {
	var (
		err        error
		start, end ast.Expression
	)

	start, err = p.constant()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Range); err != nil {
		return nil, err
	}

	end, err = p.constant()
	if err != nil {
		return nil, err
	}

	return &structured.SubRange{
		Range: &ast.Range{Start: start, End: end}}, nil
}

func (p *Parser) enumType() (*structured.Enumerated, error) {
	var err error

	if err = p.match(token.LParen); err != nil {
		return nil, err
	}

	list, err := p.identifierList()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.RParen); err != nil {
		return nil, err
	}

	return &structured.Enumerated{List: list}, nil
}

func (p *Parser) setType() (*structured.Set, error) {
	var err error

	set := &structured.Set{Token: p.lAheadToken(1)}
	if err := p.match(token.Set); err != nil {
		return nil, err
	}

	if err = p.match(token.Of); err != nil {
		return nil, err
	}

	set.BaseType, err = p.ordinalType()
	if err != nil {
		return nil, err
	}

	return set, nil
}

func (p *Parser) arrayType() (*structured.Array, error) {
	var (
		err     error
		idxType types.Ordinal
	)

	arrayType := &structured.Array{Token: p.lAheadToken(1)}
	if err = p.match(token.Array); err != nil {
		return nil, err
	}

	if err = p.match(token.LSqBrace); err != nil {
		return nil, err
	}

	idxType, err = p.indexType()
	if err != nil {
		return nil, err
	}
	arrayType.Indices = append(arrayType.Indices, idxType)

	for p.lAheadKind(1) == token.Comma {
		idxType, err = p.indexType()
		if err != nil {
			return nil, err
		}
		arrayType.Indices = append(arrayType.Indices, idxType)
	}

	if err = p.match(token.RSqBrace); err != nil {
		return nil, err
	}

	if err = p.match(token.Of); err != nil {
		return nil, err
	}

	arrayType.ComponentType, err = p.typeDenoter()
	if err != nil {
		return nil, err
	}

	return arrayType, nil
}

func (p *Parser) ordinalType() (types.Ordinal, error) {
	var (
		err     error
		idxType types.Ordinal
	)

	switch p.lAheadKind(1) {
	case token.LParen:
		if err = p.match(token.LParen); err != nil {
			return nil, err
		}

		list, err := p.identifierList()
		if err != nil {
			return nil, err
		}

		if err = p.match(token.RParen); err != nil {
			return nil, err
		}

		idxType = &structured.Enumerated{List: list}
	case token.Plus, token.Minus, token.UIntLiteral, token.CharString:
		var (
			err        error
			start, end ast.Expression
		)

		start, err = p.constant()
		if err != nil {
			return nil, err
		}

		if err = p.match(token.Range); err != nil {
			return nil, err
		}

		end, err = p.constant()
		if err != nil {
			return nil, err
		}

		idxType = &structured.SubRange{Range: &ast.Range{Start: start, End: end}}
	default:
		sym := p.curScope.Resolve(p.lAheadToken(1).Text)
		if sym == nil {
			return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
		} else if sym.GetKind() != symbols.TYPE {
			return nil, fmt.Errorf("symbol %v is not an appropriate data type", p.lAheadToken(1).Text)
		} else {
			typ, ok := sym.GetType().(types.Ordinal)
			if !ok {
				return nil, fmt.Errorf(
					"%v cannot be used as array index type. Array index type must be one of integer, Boolean, char, enum, subrange",
					p.lAheadToken(1).Text)
			}

			idxType = typ

			if err = p.consume(); err != nil {
				return nil, err
			}
		}
	}

	return idxType, nil
}

func (p *Parser) indexType() (types.Ordinal, error) {
	return p.ordinalType()
}

func (p *Parser) isNewType() bool {
	return p.lAheadKind(1) == token.LParen ||
		p.lAheadKind(1) == token.Const ||
		p.lAheadKind(1) == token.Packed ||
		p.lAheadKind(1) == token.Array ||
		p.lAheadKind(1) == token.Record ||
		p.lAheadKind(1) == token.Set ||
		p.lAheadKind(1) == token.File ||
		p.lAheadKind(1) == token.Plus ||
		p.lAheadKind(1) == token.Minus ||
		p.lAheadKind(1) == token.UIntLiteral ||
		p.lAheadKind(1) == token.CharString ||
		p.lAheadKind(1) == token.URealLiteral ||
		p.lAheadKind(1) == token.Identifier
}

// constant-definition-part = [ 'const' constant-definition ';' { constant-definition ';' } ] .
func (p *Parser) constDefinitionPart() (*ast.ConstDefinition, error) {
	var (
		err      error
		constDef *ast.ConstDef
	)

	constDefinition := &ast.ConstDefinition{Token: p.lAheadToken(1)}
	if err = p.match(token.Const); err != nil {
		return nil, err
	}

	constDef, err = p.constDefinition()
	if err != nil {
		return nil, err
	}
	constDefinition.Consts = append(constDefinition.Consts, constDef)

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	for p.lAheadKind(1) == token.Identifier {
		constDef, err = p.constDefinition()
		if err != nil {
			return nil, err
		}
		constDefinition.Consts = append(constDefinition.Consts, constDef)

		if err = p.match(token.SemiColon); err != nil {
			return nil, err
		}
	}

	return constDefinition, nil
}

// constant-definition = identifier '=' constant .
func (p *Parser) constDefinition() (*ast.ConstDef, error) {
	var err error

	constDef := &ast.ConstDef{
		Name: &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text, Scope: p.curScope}}
	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	if err = p.match(token.Equal); err != nil {
		return nil, err
	}

	constDef.Value, err = p.constant()
	if err != nil {
		return nil, err
	}

	p.curScope.Define(
		symbols.NewConstSymbol(
			constDef.Name.Name, symbols.CONST, p.getTypeOf(constDef.Value)),
	)
	return constDef, nil
}

// constant = [ sign ] ( unsigned-number | constant-identifier ) | character-string .
// constant-identifier = identifier .
func (p *Parser) constant() (ast.Expression, error) {
	var (
		err  error
		sign token.Token
		expr ast.Expression
	)

	switch p.lAheadKind(1) {
	case token.CharString:
		expr = ast.NewStringLiteral(p.lAheadToken(1), p.lAheadToken(1).Text)
	default:
		if p.isSign() {
			sign = p.lAheadToken(1)
			if err = p.consume(); err != nil {
				return nil, err
			}
		}

		if p.lAheadKind(1) == token.UIntLiteral {
			expr = &ast.UIntegerLiteral{Token: p.lAheadToken(1), Value: p.lAheadToken(1).Text}
		} else if p.lAheadKind(1) == token.URealLiteral {
			expr = &ast.URealLiteral{Token: p.lAheadToken(1), Value: p.lAheadToken(1).Text}
		} else if p.lAheadKind(1) == token.Identifier {
			sym := p.curScope.Resolve(p.lAheadToken(1).Text)
			if sym == nil {
				return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
			} else if sym.GetKind() != symbols.CONST {
				return nil, fmt.Errorf("symbol %v, is not a constant", p.lAheadToken(1).Text)
			} else {
				expr = &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text, Scope: p.curScope}
			}
		} else {
			return nil, fmt.Errorf(
				"expected unsigned number or constant, got %s instead", p.lAheadToken(1).Text)
		}

		if !reflect.DeepEqual(sign, token.Token{}) {
			expr = &ast.UnaryExpression{Operator: sign, Operand: expr}
		}
	}

	if err = p.consume(); err != nil {
		return nil, err
	}

	return expr, nil
}

// procedure-and-function-declaration-part = { ( procedure-declaration | function-declaration ) ';' } .
func (p *Parser) procedureAndFunctionDeclarationPart() ([]ast.Statement, error) {
	var callables []ast.Statement

	for p.lAheadKind(1) == token.Function || p.lAheadKind(1) == token.Procedure {
		switch p.lAheadKind(1) {
		case token.Function:
			funcDecl, err := p.functionDeclaration()
			if err != nil {
				return nil, err
			}

			callables = append(callables, funcDecl)
		case token.Procedure:
			procedureDecl, err := p.procedureDeclaration()
			if err != nil {
				return nil, err
			}

			callables = append(callables, procedureDecl)
		default:
			return nil, fmt.Errorf(
				"parse Error: expected 'procedure' or 'function', got %v instead",
				p.lAheadToken(1).Text)
		}
	}

	if err := p.match(token.SemiColon); err != nil {
		return nil, err
	}

	return callables, nil
}

// procedure-declaration := procedure-heading ';' directive | procedure-identification ';' procedure-block | procedure-heading ';' procedure-block .
// directive := letter { letter | digit } .
// procedure-block := block .
func (p *Parser) procedureDeclaration() (*ast.ProcedureDeclaration, error) {
	pHead, err := p.procedureHeading()
	if err != nil {
		return nil, err
	}

	procedureDecl := &ast.ProcedureDeclaration{Heading: pHead}
	if err := p.match(token.SemiColon); err != nil {
		return nil, err
	}

	switch p.lAheadKind(1) {
	case token.Identifier:
		procedureDecl.Directive = &ast.Identifier{
			Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text, Scope: p.curScope}
	default:
		procedureDecl.Block, err = p.block()
		if err != nil {
			return nil, err
		}
	}

	return procedureDecl, nil
}

// procedure-heading := 'procedure' identifier [ formal-parameter-list ] .
// procedure-identification := 'procedure' procedure-identifier .
// procedure-identifier := identifier .
func (p *Parser) procedureHeading() (*ast.ProcedureHeading, error) {
	var (
		err       error
		paramList []ast.FormalParameter
	)

	pHead := &ast.ProcedureHeading{Token: p.lAheadToken(1)}
	if err = p.match(token.Procedure); err != nil {
		return nil, err
	}

	pHead.Name = &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text, Scope: p.curScope}
	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	paramList, err = p.formalParameterList()
	if err != nil {
		return nil, err
	}
	pHead.Parameters = append(pHead.Parameters, paramList...)

	return pHead, nil
}

// function-heading = 'function' identifier [ formal-parameter-list ] ':' result-type .
// function-identification = 'function' function-identifier .
// function-identifier = identifier .
func (p *Parser) functionHeading() (*ast.FuncHeading, error) {
	var (
		err       error
		typ       types.Type
		paramList []ast.FormalParameter
	)

	fHead := &ast.FuncHeading{Token: p.lAheadToken(1)}
	if err = p.match(token.Function); err != nil {
		return nil, err
	}

	fHead.Name = &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text, Scope: p.curScope}
	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	paramList, err = p.formalParameterList()
	if err != nil {
		return nil, err
	}
	fHead.Parameters = append(fHead.Parameters, paramList...)

	if err = p.match(token.Colon); err != nil {
		return nil, err
	}

	typ = p.curScope.Resolve(p.lAheadToken(1).Text)
	if typ == nil {
		return nil, fmt.Errorf("parse Error: symbol %v not found", p.lAheadToken(1).Text)
	}
	fHead.ReturnType = typ

	if err = p.consume(); err != nil {
		return nil, err
	}

	return fHead, nil
}

// function-declaration := function-heading ';' directive | function-identification ';' function-block | function-heading ';' function-block .
func (p *Parser) functionDeclaration() (*ast.FuncDeclaration, error) {
	var typ types.Type

	fHead, err := p.functionHeading()
	if err != nil {
		return nil, err
	}

	funcDecl := &ast.FuncDeclaration{Heading: fHead}
	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	// define the function symbol and update the current symbol table to the new function scope
	funcName := funcDecl.Heading.Name.Name
	funcSymbol := symbols.NewFunctionSymbol(
		funcName, symbols.FUNCTION, symbols.NewLocalScope(funcName, p.curScope))
	p.curScope.Define(funcSymbol)
	funcDecl.Scope = p.curScope
	p.curScope = funcSymbol.Scope

	for _, param := range funcDecl.Heading.Parameters {
		switch pm := param.(type) {
		case *ast.ValueParam:
			if paramBuiltinType := p.curScope.Resolve(pm.Type.GetName()); paramBuiltinType != nil {
				typ = paramBuiltinType
			} else {
				// typ = some user-defined type
				// it must therefore be defined somewhere in the scope tree
				// if not found, return error
				// otherwise, typ = <<user-defined-type>>
			}

			for _, name := range pm.Names {
				p.curScope.Define(symbols.NewVariableSymbol(name.Name, symbols.VARIABLE, typ))
			}
		case *ast.VariableParam:
			if paramBuiltinType := p.curScope.Resolve(pm.Type.GetName()); paramBuiltinType != nil {
				typ = paramBuiltinType
			} else {
				// typ = some user-defined type
				// it must therefore be defined somewhere in the scope tree
				// if not found, return error
				// otherwise, typ = <<user-defined-type>>
			}

			for _, name := range pm.Names {
				p.curScope.Define(symbols.NewVariableSymbol(name.Name, symbols.VARIABLE, typ))
			}
		default:
			return nil, fmt.Errorf("%v is not ast.ValueParam or ast.VariableParam type", pm)
		}
	}
	funcSymbol.Type = funcDecl.Heading.ReturnType

	switch p.lAheadKind(1) {
	case token.Identifier:
		funcDecl.Directive = &ast.Identifier{
			Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text, Scope: p.curScope}
	default:
		funcDecl.Block, err = p.block()
		if err != nil {
			return nil, err
		}
	}

	return funcDecl, nil
}

// formal-parameter-list := '(' formal-parameter-section { ';' formal-parameter-section } ')' .
func (p *Parser) formalParameterList() ([]ast.FormalParameter, error) {
	var (
		err       error
		param     ast.FormalParameter
		paramList []ast.FormalParameter
	)

	if err = p.match(token.LParen); err != nil {
		return nil, err
	}

	param, err = p.formalParameterSection()
	if err != nil {
		return nil, err
	}
	paramList = append(paramList, param)

	for p.lAheadKind(1) == token.SemiColon {
		if err = p.consume(); err != nil {
			return nil, err
		}

		param, err = p.formalParameterSection()
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

// formal-parameter-section > value-parameter-specification | variable-parameter-specification | procedural-parameter-specification | functional-parameter-specification .
// formal-parameter-section > conformant-array-parameter-specification .
func (p *Parser) formalParameterSection() (ast.FormalParameter, error) {
	var (
		typ   types.Type
		param ast.FormalParameter
	)

	switch p.lAheadKind(1) {
	// value-parameter-specification = identifier-list ':' type-identifier .
	case token.Identifier:
		names, err := p.identifierList()
		if err != nil {
			return nil, err
		}

		if err := p.match(token.Colon); err != nil {
			return nil, err
		}

		typ, err = p.typeIdentifier()
		if err != nil {
			return nil, err
		}
		param = &ast.ValueParam{Names: names, Type: typ}

	// variable-parameter-specification = 'var' identifier-list ':' type-identifier .
	case token.Var:
		if err := p.match(token.Var); err != nil {
			return nil, err
		}

		names, err := p.identifierList()
		if err != nil {
			return nil, err
		}

		if err := p.match(token.Colon); err != nil {
			return nil, err
		}

		typ, err = p.typeIdentifier()
		if err != nil {
			return nil, err
		}
		param = &ast.VariableParam{Token: token.Var, Names: names, Type: typ}

	// procedural-parameter-specification = procedure-heading .
	case token.Procedure:
		pHead, err := p.procedureHeading()
		if err != nil {
			return nil, err
		}

		param = pHead

	// functional-parameter-specification = function-heading .
	case token.Function:
		fHead, err := p.functionHeading()
		if err != nil {
			return nil, err
		}

		param = fHead

	default:
		return nil, fmt.Errorf("parse Error: unexpected token %v", p.lAheadToken(1).Text)
	}

	return param, nil
}

// variable-declaration-part = [ 'var' variable-declaration ';' { variable-declaration ';' } ] .
func (p *Parser) variableDeclarationPart() (*ast.VarDeclaration, error) {
	var (
		err error
		d   *ast.VarDecl
	)

	varDecl := new(ast.VarDeclaration)

	if err = p.match(token.Var); err != nil {
		return nil, err
	}

	if d, err = p.variableDeclaration(); err != nil {
		return nil, err
	}
	varDecl.Token = token.Token{Kind: token.Var, Text: "var"}
	varDecl.Decls = append(varDecl.Decls, d)

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	for p.lAheadKind(1) == token.Identifier {
		if d, err = p.variableDeclaration(); err != nil {
			return nil, err
		}

		varDecl.Token = token.Token{Kind: token.Var, Text: "var"}
		varDecl.Decls = append(varDecl.Decls, d)

		if err = p.match(token.SemiColon); err != nil {
			return nil, err
		}
	}

	return varDecl, nil
}

// variable-declaration = identifier-list ':' type-denoter .
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

	varDecl.Type, err = p.typeDenoter()
	if err != nil {
		return nil, err
	}

	// add variables to symbol table
	for _, n := range names {
		p.curScope.Define(
			symbols.NewVariableSymbol(n.Name, symbols.VARIABLE, varDecl.Type))
	}

	varDecl.Scope = p.curScope

	return varDecl, nil
}

// indexed-variable = array-variable '[' index-expression, { ',' index-expression } ']' .
// array-variable = variable-access .
// index-expression = expression .
func (p *Parser) indexedVariable(arrayVar *ast.Identifier) (*ast.IndexedVariable, error) {
	var err error

	indexedVar := &ast.IndexedVariable{ArrayVar: arrayVar}
	if err = p.match(token.LSqBrace); err != nil {
		return nil, err
	}

	idxExpr, err := p.expression()
	if err != nil {
		return nil, err
	}
	indexedVar.IndexExpr = append(indexedVar.IndexExpr, idxExpr)

	for p.lAheadKind(1) == token.Comma {
		if err = p.consume(); err != nil {
			return nil, err
		}

		idxExpr, err := p.expression()
		if err != nil {
			return nil, err
		}
		indexedVar.IndexExpr = append(indexedVar.IndexExpr, idxExpr)
	}

	if err = p.match(token.RSqBrace); err != nil {
		return nil, err
	}

	return indexedVar, nil
}

// identifier-list = identifier { ',' identifier } .
func (p *Parser) identifierList() ([]*ast.Identifier, error) {
	var (
		err   error
		names []*ast.Identifier
	)

	names = append(names, &ast.Identifier{
		Token: p.lAheadToken(1),
		Name:  p.lAheadToken(1).Text,
		Scope: p.curScope})

	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	for p.lAheadKind(1) == token.Comma {
		if err = p.match(token.Comma); err != nil {
			return nil, err
		}

		names = append(names, &ast.Identifier{
			Token: p.lAheadToken(1),
			Name:  p.lAheadToken(1).Text,
			Scope: p.curScope})

		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}
	}

	return names, nil
}

// compound-statement := 'begin' statement-sequence 'end' .
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

// statement-sequence = statement { ';' statement } .
func (p *Parser) statementSequence() ([]ast.Statement, error) {
	var (
		err     error
		stmt    ast.Statement
		stmtSeq []ast.Statement
	)

	if stmt, err = p.statement(); err != nil {
		return nil, err
	}

	stmtSeq = append(stmtSeq, stmt)

	for p.lAheadKind(1) == token.SemiColon {
		if err = p.consume(); err != nil {
			return nil, err
		}

		if stmt, err = p.statement(); err != nil {
			return nil, err
		}

		stmtSeq = append(stmtSeq, stmt)
	}

	return stmtSeq, nil
}

// statement = [ label ':' ] ( simple-statement | structured-statement ) .
func (p *Parser) statement() (ast.Statement, error) {
	var (
		err  error
		stmt ast.Statement
	)

	// TODO: optional label
	// if p.lAheadKind(1) == token.UIntLiteral {

	// }

	if p.isStructuredStatement() {
		switch p.lAheadKind(1) {
		case token.If:
			stmt, err = p.ifStatement()
		case token.Begin:
			stmt, err = p.compoundStatement()
		case token.While:
			stmt, err = p.whileStatement()
		case token.With:
			stmt, err = p.withStatement()
		case token.Case:
			stmt, err = p.caseStatement()
		case token.Repeat:
			stmt, err = p.repeatStatement()
		case token.For:
			stmt, err = p.forStatement()
		default:

		}
	} else if p.isSimpleStatement() {
		stmt, err = p.simpleStatement()
	} else {
		stmt, err = nil, fmt.Errorf("parser Error: unexpected token %v", p.lAheadToken(1).Text)
	}

	return stmt, err
}

func (p *Parser) isStructuredStatement() bool {
	return p.lAheadKind(1) == token.Begin ||
		p.lAheadKind(1) == token.With ||
		p.lAheadKind(1) == token.If ||
		p.lAheadKind(1) == token.Case ||
		p.lAheadKind(1) == token.Repeat ||
		p.lAheadKind(1) == token.While ||
		p.lAheadKind(1) == token.For
}

func (p *Parser) isSimpleStatement() bool {
	return p.lAheadKind(1) == token.Identifier ||
		p.lAheadKind(1) == token.Goto ||
		p.lAheadKind(1) == token.End
}

// case-statement := 'case' case-index 'of' case-list-element { ';' case-list-element } [ ';' ] 'end' .
// case-list-element = case-constant-list ':' statement .
// case-index = expression .
func (p *Parser) caseStatement() (*ast.CaseStatement, error) {
	var err error

	caseStmt := &ast.CaseStatement{Token: p.lAheadToken(1)}
	if err = p.match(token.Case); err != nil {
		return nil, err
	}

	caseStmt.Index, err = p.expression()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Of); err != nil {
		return nil, err
	}

	element, err := p.caseListElement()
	if err != nil {
		return nil, err
	}
	caseStmt.List = append(caseStmt.List, element)

	for p.lAheadKind(1) == token.SemiColon {
		if err = p.consume(); err != nil {
			return nil, err
		}

		element, err := p.caseListElement()
		if err != nil {
			return nil, err
		}
		caseStmt.List = append(caseStmt.List, element)
	}

	if p.lAheadKind(1) == token.SemiColon {
		if err = p.consume(); err != nil {
			return nil, err
		}
	}

	if err = p.match(token.End); err != nil {
		return nil, err
	}

	return caseStmt, nil
}

// case-list-element = case-constant-list ':' statement .
func (p *Parser) caseListElement() (*ast.CaseElement, error) {
	var (
		err      error
		caseElem *ast.CaseElement
	)

	caseElem.ConstList, err = p.caseConstantList()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Colon); err != nil {
		return nil, err
	}

	caseElem.Body, err = p.statement()
	if err != nil {
		return nil, err
	}

	return caseElem, nil
}

// case-constant-list = case-constant { ',' case-constant } .
// case-constant = constant .
func (p *Parser) caseConstantList() ([]ast.Expression, error) {
	var caseConstList []ast.Expression

	constant, err := p.constant()
	if err != nil {
		return nil, err
	}
	caseConstList = append(caseConstList, constant)

	for p.lAheadKind(1) == token.Comma {
		if err = p.consume(); err != nil {
			return nil, err
		}

		constant, err := p.constant()
		if err != nil {
			return nil, err
		}
		caseConstList = append(caseConstList, constant)
	}

	return caseConstList, nil
}

// with-statement := 'with' record-variable-list 'do' statement .
func (p *Parser) withStatement() (*ast.WithStatement, error) {
	var err error

	withStmt := &ast.WithStatement{Token: p.lAheadToken(1)}
	if err = p.match(token.With); err != nil {
		return nil, err
	}

	withStmt.RecordVarList, err = p.recordVariableList()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Do); err != nil {
		return nil, err
	}

	withStmt.Body, err = p.statement()
	if err != nil {
		return nil, err
	}

	return withStmt, nil
}

// record-variable-list = record-variable { ',' record-variable } .
// record-variable = variable-access .
func (p *Parser) recordVariableList() ([]ast.Expression, error) {
	var recVarList []ast.Expression

	variable, err := p.variableAccess()
	if err != nil {
		return nil, err
	}
	recVarList = append(recVarList, variable)

	for p.lAheadKind(1) == token.Comma {
		if err := p.consume(); err != nil {
			return nil, err
		}

		variable, err := p.variableAccess()
		if err != nil {
			return nil, err
		}

		recVarList = append(recVarList, variable)
	}

	return recVarList, nil
}

// repeat-statement = 'repeat' statement-sequence 'until' Boolean-expression .
func (p *Parser) repeatStatement() (*ast.RepeatStatement, error) {
	var err error

	repeatStmt := &ast.RepeatStatement{Token: p.lAheadToken(1)}
	if err = p.match(token.Repeat); err != nil {
		return nil, err
	}

	repeatStmt.StmtSeq, err = p.statementSequence()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Until); err != nil {
		return nil, err
	}

	repeatStmt.BoolExpr, err = p.expression()
	if err != nil {
		return nil, err
	}

	return repeatStmt, nil
}

// while-statement = 'while' Boolean-expression 'do' statement .
func (p *Parser) whileStatement() (*ast.WhileStatement, error) {
	var err error

	whileStmt := &ast.WhileStatement{Token: p.lAheadToken(1)}
	if err = p.match(token.While); err != nil {
		return nil, err
	}

	whileStmt.BoolExpr, err = p.expression()
	if err != nil {
		return nil, err
	}

	if err = p.match(token.Do); err != nil {
		return nil, err
	}

	whileStmt.Body, err = p.statement()
	if err != nil {
		return nil, err
	}

	return whileStmt, nil
}

// for-statement = 'for' control-variable ':=' initial-value ( 'to' | 'downto' ) final-value 'do' statement .
// control-variable = entire-variable .
// initial-value = expression .
// final-value = expression .
func (p *Parser) forStatement() (*ast.ForStatement, error) {
	var (
		err               error
		initVal, finalVal ast.Expression
	)

	forStmt := &ast.ForStatement{Token: p.lAheadToken(1)}
	if err = p.match(token.For); err != nil {
		return nil, err
	}

	forStmt.CtrlID = &ast.Identifier{
		Token: p.lAheadToken(1),
		Name:  p.lAheadToken(1).Text,
		Scope: p.curScope,
	}
	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	if err = p.match(token.Initialize); err != nil {
		return nil, err
	}

	initVal, err = p.expression()
	if err != nil {
		return nil, err
	}
	forStmt.InitValue = initVal

	if p.lAheadKind(1) != token.To && p.lAheadKind(1) != token.DownTo {
		return nil, fmt.Errorf("expecting %v or %v; found %v",
			token.To, token.DownTo, p.lAheadToken(1).Text)
	}
	forStmt.Direction = p.lAheadKind(1)
	if err := p.consume(); err != nil {
		return nil, err
	}

	finalVal, err = p.expression()
	if err != nil {
		return nil, err
	}
	forStmt.FinalValue = finalVal

	if err = p.match(token.Do); err != nil {
		return nil, err
	}

	forStmt.Body, err = p.statement()
	if err != nil {
		return nil, err
	}

	return forStmt, err
}

// simple-statement := empty-statement | assignment-statement | procedure-statement | goto-statement .
func (p *Parser) simpleStatement() (ast.Statement, error) {
	var (
		err  error
		stmt ast.Statement
	)

	switch p.lAheadKind(1) {
	case token.End:
	case token.Identifier:
		if p.lAheadKind(2) == token.LParen {
			stmt, err = p.procedureStatement()
		} else {
			stmt, err = p.assignmentStatement()
		}
	case token.Goto:
		gotoStmt := &ast.GotoStatement{Token: p.lAheadToken(1)}
		if err = p.match(token.Goto); err != nil {
			return nil, err
		}

		gotoStmt.Label = &ast.UIntegerLiteral{Token: p.lAheadToken(1), Value: p.lAheadToken(1).Text}
		if err = p.match(token.UIntLiteral); err != nil {
			return nil, err
		}

		stmt = gotoStmt
	default:
		return nil, fmt.Errorf("expecting procedure_name, goto or assignment; found %v", p.lAheadToken(1))
	}

	return stmt, nil
}

// procedure-statement = procedure-identifier ( [ actual-parameter-list ] | read-parameter-list | readln-parameter-list | write-parameter-list | writeln-parameter-list ) .
func (p *Parser) procedureStatement() (*ast.ProcedureStatement, error) {
	// TODO: complete implementation

	var err error

	ps := ast.NewProcedureStatement(&ast.Identifier{
		Token: p.lAheadToken(1),
		Name:  p.lAheadToken(1).Text,
		Scope: p.curScope})
	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

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

// assignment-statement = ( variable-access | function-identifier ) ':=' expression .
func (p *Parser) assignmentStatement() (*ast.AssignStatement, error) {
	var (
		err error
		lhs ast.Expression
	)

	lhs, err = p.variableAccess()
	if err != nil {
		return nil, err
	}

	as := &ast.AssignStatement{Token: p.lAheadToken(1), Variable: lhs}
	if err = p.match(token.Initialize); err != nil {
		return nil, err
	}

	as.Value, err = p.expression()
	if err != nil {
		return nil, err
	}

	return as, nil
}

// expression = simple-expression [ relational-operator simple-expression ] .
func (p *Parser) expression() (ast.Expression, error) {
	simpleExpr, err := p.simpleExpression()
	if err != nil {
		return nil, err
	}

	if p.isRelationalOp() {
		relExpr := &ast.BinaryExpression{Operator: p.lAheadToken(1), Left: simpleExpr}
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

// simple-expression = [ sign ] term { adding-operator term } .
func (p *Parser) simpleExpression() (ast.Expression, error) {
	var (
		err  error
		sign token.Token
		expr ast.Expression
	)

	if p.isSign() {
		sign = p.lAheadToken(1)
		if err = p.consume(); err != nil {
			return nil, err
		}
	}

	expr, err = p.term()
	if err != nil {
		return nil, err
	}

	for p.isAddingOp() {
		binExpr := &ast.BinaryExpression{Left: expr, Operator: p.lAheadToken(1)}
		if err = p.consume(); err != nil {
			return nil, err
		}

		binExpr.Right, err = p.term()
		if err != nil {
			return nil, err
		}

		expr = binExpr
	}

	if !reflect.DeepEqual(sign, token.Token{}) {
		unaryExp := &ast.UnaryExpression{Operator: sign, Operand: expr}
		return unaryExp, nil
	}

	return expr, nil
}

// term = factor { multiplying-operator factor } .
func (p *Parser) term() (ast.Expression, error) {
	var (
		err  error
		expr ast.Expression
	)

	expr, err = p.factor()
	if err != nil {
		return nil, err
	}

	for p.isMultiplyOp() {
		binExpr := &ast.BinaryExpression{Left: expr, Operator: p.lAheadToken(1)}
		if err = p.consume(); err != nil {
			return nil, err
		}

		binExpr.Right, err = p.factor()
		if err != nil {
			return nil, err
		}

		expr = binExpr
	}

	return expr, nil
}

// factor > bound-identifier
// factor > variable-access | unsigned-constant | function-designator | set-constructor | '(' expression ')' | 'not' factor
func (p *Parser) factor() (ast.Expression, error) {
	var (
		err  error
		expr ast.Expression
	)

	switch p.lAheadKind(1) {
	case token.LSqBrace:
		var (
			err    error
			member ast.Expression
		)

		setConst := &ast.SetConstructor{Token: token.NewToken(token.Set, "set")}
		if err = p.match(token.LSqBrace); err != nil {
			return nil, err
		}

		member, err = p.memberDesignator()
		if err != nil {
			return nil, err
		}
		setConst.Members = append(setConst.Members, member)

		for p.lAheadKind(1) == token.Comma {
			if err = p.consume(); err != nil {
				return nil, err
			}

			member, err = p.memberDesignator()
			if err != nil {
				return nil, err
			}
			setConst.Members = append(setConst.Members, member)
		}

		if err = p.match(token.RSqBrace); err != nil {
			return nil, err
		}

		expr = setConst
	case token.Identifier:
		if p.lAheadKind(2) == token.LParen {
			expr, err = p.functionDesignator()
		} else {
			expr, err = p.variableAccess()
		}
	case token.UIntLiteral, token.URealLiteral, token.CharString, token.Nil:
		expr, err = p.unsignedConstant()
		if err != nil {
			return nil, err
		}
	case token.LParen:
		if err := p.match(token.LParen); err != nil {
			return nil, err
		}

		expr, err = p.expression()
		if err != nil {
			return nil, err
		}

		if err = p.match(token.RParen); err != nil {
			return nil, err
		}
	case token.Not:
		uExpr := &ast.UnaryExpression{Operator: p.lAheadToken(1)}
		if err = p.match(token.Not); err != nil {
			return nil, err
		}

		uExpr.Operand, err = p.factor()
		if err != nil {
			return nil, err
		}

		expr = uExpr
	default:
		return nil, fmt.Errorf("expected identifier or integer, got %v", p.lAheadToken(1).Text)
	}

	return expr, nil
}

func (p *Parser) memberDesignator() (ast.Expression, error) {
	var (
		err  error
		expr ast.Expression
	)

	expr, err = p.expression()
	if err != nil {
		return nil, err
	}

	if p.lAheadKind(1) == token.Range {
		if err = p.consume(); err != nil {
			return nil, err
		}

		rng := &ast.Range{Start: expr}

		expr, err = p.expression()
		if err != nil {
			return nil, err
		}
		rng.End = expr

		return rng, nil
	}

	return expr, nil
}

// variable-access := entire-variable | component-variable | identified-variable | buffer-variable .

// entire-variable = variable-identifier .
// variable-identifier = identifier .

// component-variable = indexed-variable | field-designator .
// indexed-variable = array-variable '[' index-expression, { ',' index-expression } ']' .
// array-variable = variable-access .

// field-designator = record-variable '.' field-specifier | field-designator-identifier .
// record-variable = variable-access .
func (p *Parser) variableAccess() (ast.Expression, error) {
	var (
		err  error
		expr ast.Expression
	)

	id := p.lAheadToken(1)
	sym := p.curScope.Resolve(id.Text)
	if err = p.consume(); err != nil {
		return nil, err
	}

	if sym == nil {
		return nil, fmt.Errorf("undefined symbol %v", id.Text)
	} else if sym.GetType().GetName() == "array" {
		expr, err = p.indexedVariable(&ast.Identifier{Token: id, Name: id.Text, Scope: p.curScope})
		if err != nil {
			return nil, err
		}
	} else if sym.GetType().GetName() == "record" {
		expr, err = p.fieldDesignator(&ast.Identifier{Token: id, Name: id.Text})
		if err != nil {
			return nil, err
		}
	} else if sym.GetType().GetName() == "file" {

	} else if sym.GetType().GetName() == "pointer" {

	} else {
		expr = &ast.Identifier{Token: id, Name: id.Text, Scope: p.curScope}
	}

	return expr, nil
}

// field-designator = record-variable '.' field-specifier | field-designator-identifier .
func (p *Parser) fieldDesignator(recordVar *ast.Identifier) (*ast.FieldDesignator, error) {
	fieldDesig := &ast.FieldDesignator{RecordVar: recordVar}

	if err := p.match(token.Period); err != nil {
		return nil, err
	}

	record := p.curScope.Resolve(recordVar.Name).GetType().(*structured.Record)
	for _, field := range record.FieldList {
		switch field := field.(type) {
		case *structured.FixedPart:
			for _, entry := range field.Entry {
				for _, fieldName := range entry.List {
					if fieldName.Name != p.lAheadToken(1).Text {
						continue
					}

					fieldDesig.FieldSpec = &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text}
					if err := p.match(token.Identifier); err != nil {
						return nil, err
					}

					return fieldDesig, nil
				}
			}

			return nil, fmt.Errorf("%v has no field, '%v'", record, p.lAheadToken(1).Text)
		case *structured.VariantPart:
		}
	}

	return fieldDesig, nil
}

// unsigned-constant := unsigned-number | character-string | constant-identifier | 'nil' .
// constant-identifier = identifier .
func (p *Parser) unsignedConstant() (ast.Expression, error) {
	var (
		err  error
		expr ast.Expression
	)

	switch p.lAheadKind(1) {
	case token.UIntLiteral, token.URealLiteral:
		expr, err = p.unsignedNumber()
	case token.CharString:
		expr = ast.NewStringLiteral(p.lAheadToken(1), p.lAheadToken(1).Text)
	case token.Identifier:
		expr = &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text, Scope: p.curScope}
	case token.Nil:
		expr = &ast.NilValue{Token: p.lAheadToken(1)}
	default:
		return nil, fmt.Errorf("expected unsigned constant type, instead got, %v", p.lAheadToken(1).Text)
	}

	if err = p.consume(); err != nil {
		return nil, err
	}

	return expr, nil
}

// unsigned-number := unsigned-integer | unsigned-real .
func (p *Parser) unsignedNumber() (ast.Expression, error) {
	switch p.lAheadKind(1) {
	case token.UIntLiteral:
		return &ast.UIntegerLiteral{Token: p.lAheadToken(1), Value: p.lAheadToken(1).Text}, nil
	case token.URealLiteral:
		return &ast.URealLiteral{Token: p.lAheadToken(1), Value: p.lAheadToken(1).Text}, nil
	default:
		return nil, fmt.Errorf("expected unsigned real or integer, instead got, %v", p.lAheadToken(1).Text)
	}
}

func (p *Parser) isMultiplyOp() bool {
	return p.lAheadKind(1) == token.Star ||
		p.lAheadKind(1) == token.FwdSlash ||
		p.lAheadKind(1) == token.Div ||
		p.lAheadKind(1) == token.Mod ||
		p.lAheadKind(1) == token.And
}

func (p *Parser) isAddingOp() bool {
	return p.lAheadKind(1) == token.Plus ||
		p.lAheadKind(1) == token.Minus ||
		p.lAheadKind(1) == token.Or
}

func (p *Parser) isRelationalOp() bool {
	return p.lAheadKind(1) == token.Equal ||
		p.lAheadKind(1) == token.LessThanGreaterThan ||
		p.lAheadKind(1) == token.LessThan ||
		p.lAheadKind(1) == token.GreaterThan ||
		p.lAheadKind(1) == token.LessThanOrEqual ||
		p.lAheadKind(1) == token.GreaterThanOrEqual ||
		p.lAheadKind(1) == token.In
}

func (p *Parser) isSign() bool {
	return p.lAheadKind(1) == token.Plus ||
		p.lAheadKind(1) == token.Minus
}

// if-statement := 'if' Boolean-expression 'then' statement [ else-part ] .
// Boolean-expression := expression .
func (p *Parser) ifStatement() (*ast.IfStatement, error) {
	var err error

	ifStmt := &ast.IfStatement{Token: p.lAheadToken(1)}
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

	if p.lAheadKind(1) == token.Else {
		ifStmt.ElsePath, err = p.elsePart()
		if err != nil {
			return nil, err
		}
	}

	return ifStmt, nil
}

// else-part := 'else' statement .
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

// function-designator = function-identifier [ actual-parameter-list ] .
// function-identifier = identifier .
func (p *Parser) functionDesignator() (*ast.FuncDesignator, error) {
	var err error

	funcCall := &ast.FuncDesignator{
		Name:  &ast.Identifier{Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text},
		Scope: p.curScope}

	if err = p.consume(); err != nil {
		return nil, err
	}

	funcCall.Parameters, err = p.actualParameterList()
	if err != nil {
		return nil, err
	}

	return funcCall, nil
}

// actual-parameter-list = '(' actual-parameter { ',' actual-parameter } ')' .
func (p *Parser) actualParameterList() ([]ast.Expression, error) {
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

	for p.lAheadKind(1) == token.Comma {
		if err := p.consume(); err != nil {
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

// actual-parameter := expression | variable-access | procedure-identifier | function-identifier .
func (p *Parser) actualParameter() (ast.Expression, error) {
	var (
		err  error
		expr ast.Expression
	)

	if p.lAheadKind(1) == token.Identifier {
		sym := p.curScope.Resolve(p.lAheadToken(1).Text)
		if sym == nil {
			return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
		}

		if sym.GetKind() == symbols.FUNCTION || sym.GetKind() == symbols.PROCEDURE {
			return &ast.Identifier{
				Token: p.lAheadToken(1), Name: p.lAheadToken(1).Text}, nil
		}
	}

	expr, err = p.expression()
	if err != nil {
		return nil, err
	}

	return expr, nil
}

// writeln-parameter-list = [ '(' ( file-variable | write-parameter ) { ',' write-parameter } ')' ] .
func (p *Parser) writelnParameterList() ([]ast.Expression, error) {
	var (
		err              error
		writeParam       *ast.WriteParameter
		writelnParamList []ast.Expression
	)

	if p.lAheadKind(1) == token.LParen {
		if err = p.match(token.LParen); err != nil {
			return nil, err
		}

		// TODO: implement file-variable alternative
		writeParam, err = p.writeParameter()
		if err != nil {
			return nil, err
		}
		writelnParamList = append(writelnParamList, writeParam)

		for p.lAheadKind(1) == token.Comma {
			if err = p.consume(); err != nil {
				return nil, err
			}

			writeParam, err = p.writeParameter()
			if err != nil {
				return nil, err
			}
			writelnParamList = append(writelnParamList, writeParam)
		}

		if err = p.match(token.RParen); err != nil {
			return nil, err
		}
	}

	return writelnParamList, nil
}

// write-parameter := expression [ ':' expression [ ':' expression ] ] .
func (p *Parser) writeParameter() (*ast.WriteParameter, error) {
	var (
		err  error
		expr ast.Expression
	)

	expr, err = p.expression()
	if err != nil {
		return nil, err
	}

	wp := &ast.WriteParameter{E: expr}

	if p.lAheadKind(1) == token.Colon {
		if err = p.match(token.Colon); err != nil {
			return nil, err
		}

		wp.TotalWidth, err = p.expression()
		if err != nil {
			return nil, err
		}

		if p.lAheadKind(1) == token.Colon {
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
