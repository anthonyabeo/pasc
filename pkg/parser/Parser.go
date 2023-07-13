package parser

import (
	"fmt"
	"strconv"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/semantics"
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
	symTable  *semantics.WonkySymbolTable
	curBlock  *ast.Block
}

// NewParser constructs and returns an instance of parser
func NewParser(lexer Lexer, symTab *semantics.WonkySymbolTable) (*Parser, error) {
	parser := Parser{input: lexer, k: 3, idx: 0, symTable: symTab}
	for i := 0; i < parser.k; i++ {
		if err := parser.consume(); err != nil {
			return nil, err
		}
	}

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

	program := &ast.Program{Name: programName, ParamList: programParams, TokenKind: token.Program}

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

	programName = &ast.Identifier{TokenKind: token.Identifier, Name: p.lAheadToken(1).Text}

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

	if p.lAheadKind(1) == token.Label {
		labelDefinition, err = p.labelDeclarationPart()
		if err != nil {
			return nil, err
		}
	}

	if p.lAheadKind(1) == token.Const {
		constDefinition, err = p.constDefinitionPart()
		if err != nil {
			return nil, err
		}
	}

	if p.lAheadKind(1) == token.Type {
		typeDefinition, err = p.typeDefinitionPart()
		if err != nil {
			return nil, err
		}
	}

	if p.lAheadKind(1) == token.Var {
		varDecl, err = p.variableDeclarationPart()
		if err != nil {
			return nil, err
		}
	}

	callables, err = p.procedureAndFunctionDeclarationPart()
	if err != nil {
		return nil, err
	}

	compoundStmt, err = p.compoundStatement()
	if err != nil {
		return nil, err
	}

	block.Types = typeDefinition
	block.VarDeclaration = varDecl
	block.Consts = constDefinition
	block.Labels = labelDefinition
	block.Stats = append(block.Stats, compoundStmt.Statements...)
	block.Callables = append(block.Callables, callables...)

	return block, nil
}

// label-declaration-part = [ 'label' label { ',' label } ';' ] .
func (p *Parser) labelDeclarationPart() (*ast.LabelDefinition, error) {
	labelDefinition := &ast.LabelDefinition{Token: p.lAheadToken(1)}
	if err := p.match(token.Label); err != nil {
		return nil, err
	}

	label := &ast.UIntegerLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}
	if err := p.match(token.UIntLiteral); err != nil {
		return nil, err
	}
	labelDefinition.Labels = append(labelDefinition.Labels, label)

	if p.symTable.DeclaredLocally(label.Value) {
		panic(fmt.Sprintf("%s already declared in this scope", label.Value))
	}

	p.symTable.EnterSymbol(label.Value, semantics.NewLabel(label.Value, semantics.LABEL, nil))

	for p.lAheadKind(1) == token.Comma {
		if err := p.consume(); err != nil {
			return nil, err
		}

		label := &ast.UIntegerLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}
		if err := p.match(token.UIntLiteral); err != nil {
			return nil, err
		}
		labelDefinition.Labels = append(labelDefinition.Labels, label)

		if p.symTable.DeclaredLocally(label.Value) {
			panic(fmt.Sprintf("%s already declared in this scope", label.Value))
		}

		p.symTable.EnterSymbol(label.Value, semantics.NewLabel(label.Value, semantics.LABEL, nil))
	}

	if err := p.match(token.SemiColon); err != nil {
		return nil, err
	}

	return labelDefinition, nil
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

	typeDef = &ast.TypeDef{Name: &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}}
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

	if p.symTable.DeclaredLocally(typeDef.Name.Name) {
		panic(fmt.Sprintf("%s already declared in this scope", typeDef.Name.Name))
	}

	p.symTable.EnterSymbol(typeDef.Name.Name, semantics.NewTypeDef(typeDef.Name.Name, semantics.TYPE, typeDef.TypeDenoter))

	return typeDef, nil
}

func (p *Parser) typeIdentifier() (types.Type, error) {
	var (
		err error
		typ types.Type
	)

	sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
	if sym == nil {
		return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
	} else if sym.Kind() != semantics.TYPE {
		return nil, fmt.Errorf("symbol %v is not an appropriate data type", p.lAheadToken(1).Text)
	} else {
		if err = p.consume(); err != nil {
			return nil, err
		}

		typ = sym.Type()
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
				enumTyp, err := p.enumType()
				if err != nil {
					return nil, err
				}

				for index, enum := range enumTyp.List {
					if p.symTable.DeclaredLocally(enum.Name) {
						panic(fmt.Sprintf("%s already declared in this scope", enum.Name))
					}

					p.symTable.EnterSymbol(
						enum.Name,
						semantics.NewConst(enum.Name, semantics.CONST, enumTyp, &ast.UIntegerLiteral{Value: strconv.Itoa(index)}))
				}

				typ = enumTyp
			case token.Record:
				typ, err = p.recordType()
				if err != nil {
					return nil, err
				}
			case token.File:
				typ, err = p.fileType()
				if err != nil {
					return nil, err
				}
			case token.Set:
				typ, err = p.setType()
				if err != nil {
					return nil, err
				}
			case token.Plus, token.Minus, token.UIntLiteral, token.StrLiteral, token.URealLiteral, token.Identifier:
				typ, err = p.subRangeType()
				if err != nil {
					return nil, err
				}
			case token.Packed:
			case token.Caret:
				typ, err = p.pointerType()
				if err != nil {
					return nil, err
				}
			}
		}
	}

	return typ, nil
}

func (p *Parser) pointerType() (*structured.Pointer, error) {
	var err error

	ptr := &structured.Pointer{TokenKind: p.lAheadKind(1)}
	if err = p.match(token.Caret); err != nil {
		return nil, err
	}

	ptr.DomainType, err = p.typeIdentifier()
	if err != nil {
		return nil, err
	}

	return ptr, nil
}

func (p *Parser) fileType() (*structured.File, error) {
	var err error

	file := &structured.File{TokenKind: p.lAheadKind(1)}
	if err = p.match(token.File); err != nil {
		return nil, err
	}

	if err = p.match(token.Of); err != nil {
		return nil, err
	}

	file.ComponentType, err = p.typeDenoter()
	if err != nil {
		return nil, err
	}

	return file, nil
}

// record-type = 'record' field-list 'end' .
func (p *Parser) recordType() (*structured.Record, error) {
	var err error

	record := &structured.Record{TokenKind: p.lAheadKind(1)}

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

	offset := 0
	for _, field := range record.FieldList {
		switch f := field.(type) {
		case *structured.FixedPart:
			for _, rs := range f.Entry {
				for _, entry := range rs.List {
					if p.symTable.DeclaredLocally(entry.Name) {
						panic(fmt.Sprintf("cannot redeclare field name '%s'", entry.Name))
					}

					p.symTable.EnterSymbol(
						entry.Name, semantics.NewField(entry.Name, semantics.FIELD, rs.Type, uint64(offset)))
					offset++
				}
			}
		case *structured.VariantPart:
		}
	}

	return record, nil
}

// field-list = [ ( fixed-part [ ';' variant-part ] | variant-part ) [ ';' ] ] .
func (p *Parser) fieldList() ([]structured.Field, error) {
	var (
		err       error
		fieldList []structured.Field
	)

	fixedPart := &structured.FixedPart{}
	varPart := &structured.VariantPart{}

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

		for p.lAheadKind(1) == token.SemiColon {
			if err = p.consume(); err != nil {
				return nil, err
			}

			if p.lAheadKind(1) == token.Case {
				varPart, err = p.variantPart()
				if err != nil {
					return nil, err
				}
			} else {
				recordSec, err := p.recordSection()
				if err != nil {
					return nil, err
				}
				fixedPart.Entry = append(fixedPart.Entry, recordSec)
			}
		}
	}

	if p.lAheadKind(1) == token.SemiColon {
		if err = p.consume(); err != nil {
			return nil, err
		}
	}

	fieldList = append(fieldList, fixedPart, varPart)

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
		selector.TagField = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
		if err = p.consume(); err != nil {
			return nil, err
		}

		if err = p.match(token.Colon); err != nil {
			return nil, err
		}
	}

	sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
	if sym == nil {
		return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
	} else if sym.Kind() != semantics.TYPE {
		return nil, fmt.Errorf("symbol %v is not an appropriate data type", p.lAheadToken(1).Text)
	} else {
		typ, ok := sym.Type().(types.Ordinal)
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
		Range:    &ast.Range{Start: start, End: end},
		HostType: start.Type(),
	}, nil
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

func (p *Parser) setType() (*types.Set, error) {
	var err error

	set := &types.Set{TokenKind: p.lAheadKind(1)}
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

func (p *Parser) arrayType() (*types.Array, error) {
	var (
		err     error
		idxType types.Ordinal
	)

	arrayType := &types.Array{TokenKind: p.lAheadKind(1)}
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
		if err = p.consume(); err != nil {
			return nil, err
		}

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
	case token.Plus, token.Minus, token.UIntLiteral, token.StrLiteral:
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

		idxType = &structured.SubRange{Range: &ast.Range{Start: start, End: end} /*HostType: p.getTypeOf(start)*/}
	default:
		sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
		if sym == nil {
			return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
		} else if sym.Kind() != semantics.TYPE {
			return nil, fmt.Errorf("symbol %v is not an appropriate data type", p.lAheadToken(1).Text)
		} else {
			typ, ok := sym.Type().(types.Ordinal)
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
		p.lAheadKind(1) == token.StrLiteral ||
		p.lAheadKind(1) == token.URealLiteral ||
		p.lAheadKind(1) == token.Identifier ||
		p.lAheadKind(1) == token.Caret
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

	constDef := &ast.ConstDef{Name: &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}}
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

	if p.symTable.DeclaredLocally(constDef.Name.Name) {
		panic(fmt.Sprintf("symbol '%v' already defined as type '%v'", constDef.Name.Name, constDef.Value.Type()))
	}

	p.symTable.EnterSymbol(
		constDef.Name.Name, semantics.NewConst(constDef.Name.Name, semantics.CONST, constDef.Value.Type(), constDef.Value))

	return constDef, nil
}

// constant = [ sign ] ( unsigned-number | constant-identifier ) | character-string .
// constant-identifier = identifier .
func (p *Parser) constant() (ast.Expression, error) {
	var (
		err  error
		sign token.Kind
		expr ast.Expression
	)

	switch p.lAheadKind(1) {
	case token.StrLiteral:
		expr = &ast.StrLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}
	default:
		if p.isSign() {
			sign = p.lAheadKind(1)
			if err = p.consume(); err != nil {
				return nil, err
			}
		}

		if p.lAheadKind(1) == token.UIntLiteral {
			expr = &ast.UIntegerLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}
		} else if p.lAheadKind(1) == token.URealLiteral {
			expr = &ast.URealLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}
		} else if p.lAheadKind(1) == token.Identifier {
			sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
			if sym == nil {
				return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
			} else if sym.Kind() != semantics.CONST {
				return nil, fmt.Errorf("symbol %v, is not a constant", p.lAheadToken(1).Text)
			} else {
				expr = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
			}
		} else {
			return nil, fmt.Errorf(
				"expected unsigned number or constant, got %s instead", p.lAheadToken(1).Text)
		}

		if sign == token.Minus || sign == token.Plus {
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

		if err := p.match(token.SemiColon); err != nil {
			return nil, err
		}
	}

	return callables, nil
}

// procedure-declaration := procedure-heading ';' directive | procedure-identification ';' procedure-block | procedure-heading ';' procedure-block .
// directive := letter { letter | digit } .
// procedure-block := block .
func (p *Parser) procedureDeclaration() (*ast.ProcedureDeclaration, error) {
	var typ types.Type

	pHead, err := p.procedureHeading()
	if err != nil {
		return nil, err
	}

	procedureDecl := &ast.ProcedureDeclaration{Heading: pHead}
	if err := p.match(token.SemiColon); err != nil {
		return nil, err
	}

	p.symTable.EnterSymbol(
		procedureDecl.Heading.PName.Name,
		semantics.NewProcedure(procedureDecl.Heading.PName.Name, semantics.PROCEDURE, pHead))

	p.symTable.OpenScope()

	for _, param := range procedureDecl.Heading.Parameters {
		switch pm := param.(type) {
		case *ast.ValueParam:
			sym := p.symTable.RetrieveSymbol(pm.Typ.Name())
			if sym == nil {
				return nil, fmt.Errorf("symbol '%v' is not defined", pm.Typ.Name())
			} else if sym.Kind() != semantics.TYPE {
				return nil, fmt.Errorf("'%v' is not a known type", pm.Typ.Name())
			} else {
				typ = sym.Type()
			}

			for _, name := range pm.Names {
				p.symTable.EnterSymbol(name.Name, semantics.NewVariable(name.Name, semantics.VARIABLE, typ))
				if err != nil {
					return nil, err
				}
			}
		case *ast.VariableParam:
			sym := p.symTable.RetrieveSymbol(pm.Typ.Name())
			if sym == nil {
				return nil, fmt.Errorf("symbol '%v' is not defined", pm.Typ.Name())
			} else if sym.Kind() != semantics.TYPE {
				return nil, fmt.Errorf("'%v' is not a known type", pm.Typ.Name())
			} else {
				typ = sym.Type()
			}

			for _, name := range pm.Names {
				p.symTable.EnterSymbol(name.Name, semantics.NewVariable(name.Name, semantics.VARIABLE, typ))
				if err != nil {
					return nil, err
				}
			}
		case *ast.FuncHeading:
			if p.symTable.DeclaredLocally(pm.FName.Name) {
				return nil, fmt.Errorf("'%s' already declared", pm.FName.Name)
			}

			p.symTable.EnterSymbol(
				pm.FName.Name,
				semantics.NewFunction(pm.FName.Name, semantics.FUNCTION, pm))

		case *ast.ProcedureHeading:
		default:
			return nil, fmt.Errorf("%v is not ast.ValueParam or ast.VariableParam type", pm)
		}
	}

	switch p.lAheadKind(1) {
	case token.Identifier:
		procedureDecl.Directive = &ast.Identifier{
			TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
	default:
		procedureDecl.Block, err = p.block()
		if err != nil {
			return nil, err
		}
	}

	p.symTable.CloseScope()

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

	pHead := &ast.ProcedureHeading{TokenKind: p.lAheadKind(1)}
	if err = p.match(token.Procedure); err != nil {
		return nil, err
	}

	pHead.PName = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	if p.lAheadKind(1) == token.LParen {
		paramList, err = p.formalParameterList()
		if err != nil {
			return nil, err
		}

		pHead.Parameters = append(pHead.Parameters, paramList...)
	}

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

	fHead := &ast.FuncHeading{TokenKind: p.lAheadKind(1)}
	if err = p.match(token.Function); err != nil {
		return nil, err
	}

	fHead.FName = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
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

	sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
	if sym == nil {
		return nil, fmt.Errorf("parse Error: symbol %v not found", p.lAheadToken(1).Text)
	} else if sym.Kind() != semantics.TYPE {
		return nil, fmt.Errorf("expected %v to be a type", sym.Name())
	} else {
		typ = sym.Type()
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

	funcName := funcDecl.Heading.FName.Name
	funcSymbol := semantics.NewFunction(funcName, semantics.FUNCTION, fHead)
	p.symTable.EnterSymbol(funcName, funcSymbol)

	p.symTable.OpenScope()

	for _, param := range funcDecl.Heading.Parameters {
		switch pm := param.(type) {
		case *ast.ValueParam:
			sym := p.symTable.RetrieveSymbol(pm.Typ.Name())
			if sym == nil {
				return nil, fmt.Errorf("symbol '%v' is not defined", pm.Typ.Name())
			} else if sym.Kind() != semantics.TYPE {
				return nil, fmt.Errorf("'%v' is not a known type", pm.Typ.Name())
			} else {
				typ = sym.Type()
			}

			for _, name := range pm.Names {
				p.symTable.EnterSymbol(name.Name, semantics.NewVariable(name.Name, semantics.VARIABLE, typ))
				if err != nil {
					return nil, err
				}
			}
		case *ast.VariableParam:
			sym := p.symTable.RetrieveSymbol(pm.Typ.Name())
			if sym == nil {
				return nil, fmt.Errorf("symbol '%v' is not defined", pm.Typ.Name())
			} else if sym.Kind() != semantics.TYPE {
				return nil, fmt.Errorf("'%v' is not a known type", pm.Typ.Name())
			} else {
				typ = sym.Type()
			}

			for _, name := range pm.Names {
				p.symTable.EnterSymbol(name.Name, semantics.NewVariable(name.Name, semantics.VARIABLE, typ))
				if err != nil {
					return nil, err
				}
			}
		default:
			return nil, fmt.Errorf("%v is not ast.ValueParam or ast.VariableParam type", pm)
		}
	}

	switch p.lAheadKind(1) {
	case token.Identifier:
		funcDecl.Directive = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
	default:
		funcDecl.Block, err = p.block()
		if err != nil {
			return nil, err
		}
	}

	p.symTable.CloseScope()

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

		param = &ast.ValueParam{Names: names, Typ: typ}
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

		param = &ast.VariableParam{Token: token.Var, Names: names, Typ: typ}
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
	varDecl.TokenKind = token.Var
	varDecl.Decls = append(varDecl.Decls, d)

	if err = p.match(token.SemiColon); err != nil {
		return nil, err
	}

	for p.lAheadKind(1) == token.Identifier {
		if d, err = p.variableDeclaration(); err != nil {
			return nil, err
		}

		varDecl.TokenKind = token.Var
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
		if p.symTable.DeclaredLocally(n.Name) {
			panic(fmt.Sprintf("symbol '%v' already defined as type '%v'", n.Name, varDecl.Type))
		}

		p.symTable.EnterSymbol(n.Name, semantics.NewVariable(n.Name, semantics.VARIABLE, varDecl.Type))
	}

	return varDecl, nil
}

// indexed-variable = array-variable '[' index-expression, { ',' index-expression } ']' .
// array-variable = variable-access .
// index-expression = expression .
func (p *Parser) indexedVariable() (*ast.IndexedVariable, error) {
	var err error

	arrayVar := &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
	if err = p.consume(); err != nil {
		return nil, err
	}

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
		TokenKind: p.lAheadKind(1),
		Name:      p.lAheadToken(1).Text})

	if err = p.match(token.Identifier); err != nil {
		return nil, err
	}

	for p.lAheadKind(1) == token.Comma {
		if err = p.match(token.Comma); err != nil {
			return nil, err
		}

		names = append(names, &ast.Identifier{
			TokenKind: p.lAheadKind(1),
			Name:      p.lAheadToken(1).Text,
		})

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
		err   error
		label string
		stmt  ast.Statement
	)

	if p.lAheadKind(1) == token.UIntLiteral {
		label = p.lAheadToken(1).Text
		if err = p.consume(); err != nil {
			return nil, err
		}

		if err = p.match(token.Colon); err != nil {
			return nil, err
		}
	}

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

	if label != "" {
		stmt.SetLabel(label)
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

	caseStmt := &ast.CaseStatement{TokenKind: p.lAheadKind(1)}
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
		err error
	)

	caseElem := new(ast.CaseElement)
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

	withStmt := &ast.WithStatement{TokenKind: p.lAheadKind(1)}
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

	sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
	if sym == nil {
		return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
	} else if sym.Type().Name() != "record" {
		return nil, fmt.Errorf("expected '%s' to be record-type, got '%s' instead",
			p.lAheadToken(1).Text, sym.Type().Name())
	} else {
		recVarList = append(recVarList, &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text})
	}

	if err := p.consume(); err != nil {
		return nil, err
	}

	for p.lAheadKind(1) == token.Comma {
		if err := p.consume(); err != nil {
			return nil, err
		}

		sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
		if sym == nil {
			return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
		} else if sym.Type().Name() != "record" {
			return nil, fmt.Errorf("expected '%s' to be record-type, got '%s' instead",
				p.lAheadToken(1).Text, sym.Type().Name())
		} else {
			recVarList = append(recVarList, &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text})
		}

		if err := p.consume(); err != nil {
			return nil, err
		}
	}

	return recVarList, nil
}

// repeat-statement = 'repeat' statement-sequence 'until' Boolean-expression .
func (p *Parser) repeatStatement() (*ast.RepeatStatement, error) {
	var err error

	repeatStmt := &ast.RepeatStatement{TokenKind: p.lAheadKind(1)}
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

	whileStmt := &ast.WhileStatement{TokenKind: p.lAheadKind(1)}
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

	forStmt := &ast.ForStatement{TokenKind: p.lAheadKind(1)}
	if err = p.match(token.For); err != nil {
		return nil, err
	}

	forStmt.CtrlID = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
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

		if err != nil {
			return nil, err
		}
	case token.Goto:
		gotoStmt := &ast.GotoStatement{TokenKind: p.lAheadKind(1)}
		if err = p.match(token.Goto); err != nil {
			return nil, err
		}

		gotoStmt.Label = &ast.UIntegerLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}
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
func (p *Parser) procedureStatement() (ast.ProcedureStatement, error) {
	var (
		err error
		ps  ast.ProcedureStatement
	)

	switch p.lAheadToken(1).Text {
	case "writeln":
		wln := &ast.Writeln{Name: p.lAheadToken(1).Text}
		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}

		wln.File, wln.ParamList, err = p.writelnParameterList()
		if err != nil {
			return nil, err
		}

		ps = wln
	case "write":
		write := &ast.Write{Name: p.lAheadToken(1).Text}
		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}

		write.File, write.ParamList, err = p.writeParameterList()
		if err != nil {
			return nil, err
		}

		ps = write
	case "readln":
		readLn := &ast.ReadLn{Name: p.lAheadToken(1).Text}
		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}

		readLn.File, readLn.VarAccess, err = p.readLnParameterList()
		if err != nil {
			return nil, err
		}

		ps = readLn
	case "read":
		read := &ast.Read{Name: p.lAheadToken(1).Text}
		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}

		read.File, read.VarAccess, err = p.readParameterList()
		if err != nil {
			return nil, err
		}

		ps = read
	default:
		stmt := &ast.ProcedureStmt{
			Name: &ast.Identifier{
				TokenKind: p.lAheadKind(1),
				Name:      p.lAheadToken(1).Text,
			},
		}
		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}

		stmt.Args, err = p.actualParameterList()
		if err != nil {
			return nil, err
		}

		ps = stmt
	}

	return ps, nil
}

// assignment-statement = ( variable-access | function-identifier ) ':=' expression .
func (p *Parser) assignmentStatement() (ast.Statement, error) {
	var (
		err error
		lhs ast.Expression
	)

	sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
	if sym != nil && sym.Kind() == semantics.FUNCTION {
		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}

		ret := &ast.ReturnStatement{TokenKind: token.Return}
		if err = p.match(token.Initialize); err != nil {
			return nil, err
		}

		ret.Expr, err = p.expression()
		if err != nil {
			return nil, err
		}

		return ret, nil
	}

	lhs, err = p.variableAccess()
	if err != nil {
		return nil, err
	}

	as := &ast.AssignStatement{TokenKind: p.lAheadKind(1), Variable: lhs}
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
		relExpr := &ast.BinaryExpression{Operator: p.lAheadKind(1), Left: simpleExpr}
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
		sign token.Kind
		expr ast.Expression
	)

	if p.isSign() {
		sign = p.lAheadKind(1)
		if err = p.consume(); err != nil {
			return nil, err
		}
	}

	expr, err = p.term()
	if err != nil {
		return nil, err
	}

	for p.isAddingOp() {
		binExpr := &ast.BinaryExpression{Left: expr, Operator: p.lAheadKind(1)}
		if err = p.consume(); err != nil {
			return nil, err
		}

		binExpr.Right, err = p.term()
		if err != nil {
			return nil, err
		}

		expr = binExpr
	}

	if sign == token.Minus || sign == token.Plus {
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
		binExpr := &ast.BinaryExpression{Left: expr, Operator: p.lAheadKind(1)}
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

		setConst := &ast.SetConstructor{TokenKind: token.Set}
		if err = p.match(token.LSqBrace); err != nil {
			return nil, err
		}

		if p.lAheadKind(1) != token.RSqBrace {
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
	case token.UIntLiteral, token.URealLiteral, token.StrLiteral, token.Nil, token.True, token.False:
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
		uExpr := &ast.UnaryExpression{Operator: p.lAheadKind(1)}
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
func (p *Parser) variableAccess() (ast.Expression, error) {
	var (
		err  error
		expr ast.Expression
	)

	sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
	if sym == nil {
		return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
	} else if sym.Type().Name() == "array" {
		iExpr, err := p.indexedVariable()
		if err != nil {
			return nil, err
		}

		for p.lAheadKind(1) == token.LSqBrace {
			if err = p.consume(); err != nil {
				return nil, err
			}

			ie, err := p.expression()
			if err != nil {
				return nil, err
			}

			iExpr.IndexExpr = append(iExpr.IndexExpr, ie)

			if err = p.match(token.RSqBrace); err != nil {
				return nil, err
			}
		}

		expr = iExpr
	} else if sym.Type().Name() == "record" {
		recVar := &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
		if err = p.consume(); err != nil {
			return nil, err
		}

		expr, err = p.fieldDesignator(recVar)
		if err != nil {
			return nil, err
		}
	} else if sym.Type().Name() == "file" {

	} else if sym.Type().Name() == "pointer" {
		expr, err = p.identifiedVariable()
		if err != nil {
			return nil, err
		}

		if p.lAheadKind(1) == token.Period {
			expr, err = p.fieldDesignator(expr)
			if err != nil {
				return nil, err
			}
		}
	} else {
		expr = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
		if err = p.consume(); err != nil {
			return nil, err
		}
	}

	return expr, nil
}

func (p *Parser) identifiedVariable() (*ast.IdentifiedVariable, error) {
	var (
		err   error
		idVar *ast.IdentifiedVariable
	)

	sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
	ptr := sym.Type().(*structured.Pointer)

	idVar = &ast.IdentifiedVariable{
		PointerName: &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text},
		UnderType:   ptr.DomainType,
	}
	if err = p.consume(); err != nil {
		return nil, err
	}

	if err = p.match(token.Caret); err != nil {
		return nil, err
	}

	return idVar, nil
}

// field-designator = record-variable '.' field-specifier | field-designator-identifier .
func (p *Parser) fieldDesignator(recVar ast.Expression) (*ast.FieldDesignator, error) {
	var (
		ok  bool
		err error
	)

	fieldDes := &ast.FieldDesignator{RecordVar: recVar}
	if err = p.match(token.Period); err != nil {
		return nil, err
	}

	record, ok := p.symTable.RetrieveSymbol(recVar.String()).Type().(*structured.Record)
	if !ok {
		record, ok = recVar.(*ast.IdentifiedVariable).UnderType.(*structured.Record)
		if !ok {
			return nil, fmt.Errorf(
				"expected variable to be record type or pointer to record type. got %s instead",
				record)
		}
	}

	field := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
	if field == nil {
		return nil, fmt.Errorf("record type '%v' has no field named '%v'", recVar.String(), p.lAheadToken(1).Text)
	} else if field.Kind() != semantics.FIELD {
		return nil, fmt.Errorf("%v is not a field of record type %v", p.lAheadToken(1).Text, recVar)
	} else {
		fieldDes.FieldSpec = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
		if err = p.match(token.Identifier); err != nil {
			return nil, err
		}
	}

	return fieldDes, nil
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
	case token.StrLiteral:
		expr = &ast.StrLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}
	case token.Identifier:
		expr = &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text /*Scope: p.curScope*/}
	case token.Nil:
		expr = &ast.NilValue{TokenKind: p.lAheadKind(1)}
	case token.True, token.False:
		expr = &ast.BoolLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}
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
		return &ast.UIntegerLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}, nil
	case token.URealLiteral:
		return &ast.URealLiteral{TokenKind: p.lAheadKind(1), Value: p.lAheadToken(1).Text}, nil
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

	ifStmt := &ast.IfStatement{TokenKind: p.lAheadKind(1)}
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
		Name: &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}}

	if err = p.consume(); err != nil {
		return nil, err
	}

	funcCall.Args, err = p.actualParameterList()
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
		sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
		if sym == nil {
			return nil, fmt.Errorf("undefined symbol %v", p.lAheadToken(1).Text)
		}

		if sym.Kind() == semantics.FUNCTION || sym.Kind() == semantics.PROCEDURE {
			id := &ast.Identifier{TokenKind: p.lAheadKind(1), Name: p.lAheadToken(1).Text}
			if err = p.consume(); err != nil {
				return nil, err
			}

			return id, nil
		}
	}

	expr, err = p.expression()
	if err != nil {
		return nil, err
	}

	return expr, nil
}

// writeln-parameter-list = [ '(' ( file-variable | write-parameter ) { ',' write-parameter } ')' ] .
func (p *Parser) writelnParameterList() (*ast.Identifier, []ast.Expression, error) {
	var (
		err              error
		file             *ast.Identifier
		writeParam       *ast.WriteParameter
		writelnParamList []ast.Expression
	)

	if p.lAheadKind(1) == token.LParen {
		if err = p.match(token.LParen); err != nil {
			return nil, nil, err
		}

		sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
		if sym != nil && sym.Type().Name() == "file" {
			file = &ast.Identifier{TokenKind: token.File, Name: sym.Name()}
		} else {
			writeParam, err = p.writeParameter()
			if err != nil {
				return nil, nil, err
			}
			writelnParamList = append(writelnParamList, writeParam)
		}

		for p.lAheadKind(1) == token.Comma {
			if err = p.consume(); err != nil {
				return nil, nil, err
			}

			writeParam, err = p.writeParameter()
			if err != nil {
				return nil, nil, err
			}
			writelnParamList = append(writelnParamList, writeParam)
		}

		if err = p.match(token.RParen); err != nil {
			return nil, nil, err
		}
	}

	file = &ast.Identifier{TokenKind: token.Output, Name: "output"}

	return file, writelnParamList, nil
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

// write-parameter-list = '(' [ file-variable ',' ] write-parameter {',' write-parameter } ')' .
func (p *Parser) writeParameterList() (*ast.Identifier, []ast.Expression, error) {
	var (
		err            error
		file           *ast.Identifier
		writeParam     *ast.WriteParameter
		writeParamList []ast.Expression
	)

	if err = p.match(token.LParen); err != nil {
		return nil, nil, err
	}

	if p.lAheadKind(1) == token.Identifier {
		sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
		if sym != nil {
			return nil, nil, fmt.Errorf("undefined symbol %s", p.lAheadToken(1).Text)
		} else if sym.Type().Name() != "file" {
			return nil, nil, fmt.Errorf("expected %s to be file-type, it is of a '%s' type",
				p.lAheadToken(1).Text, sym.Type())
		} else {
			file = &ast.Identifier{TokenKind: token.File, Name: sym.Name()}
		}
	}

	writeParam, err = p.writeParameter()
	if err != nil {
		return nil, nil, err
	}
	writeParamList = append(writeParamList, writeParam)

	for p.lAheadKind(1) == token.Comma {
		if err = p.consume(); err != nil {
			return nil, nil, err
		}

		writeParam, err = p.writeParameter()
		if err != nil {
			return nil, nil, err
		}
		writeParamList = append(writeParamList, writeParam)
	}

	if err = p.match(token.RParen); err != nil {
		return nil, nil, err
	}

	return file, writeParamList, nil
}

// read-parameter-list = '(' [ file-variable ',' ] variable-access { ',' variable-access } ')' .
func (p *Parser) readParameterList() (*ast.Identifier, []ast.Expression, error) {
	var (
		err               error
		file              *ast.Identifier
		expr              ast.Expression
		readParameterList []ast.Expression
	)

	if err = p.match(token.LParen); err != nil {
		return nil, nil, err
	}

	if p.lAheadKind(1) == token.Identifier {
		sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
		if sym != nil {
			return nil, nil, fmt.Errorf("undefined symbol %s", p.lAheadToken(1).Text)
		} else if sym.Type().Name() != "file" {
			return nil, nil, fmt.Errorf("expected %s to be file-type, it is of a '%s' type",
				p.lAheadToken(1).Text, sym.Type())
		} else {
			file = &ast.Identifier{TokenKind: token.File, Name: sym.Name()}
		}
	}

	expr, err = p.variableAccess()
	if err != nil {
		return nil, nil, err
	}
	readParameterList = append(readParameterList, expr)

	for p.lAheadKind(1) == token.Comma {
		if err = p.consume(); err != nil {
			return nil, nil, err
		}

		expr, err = p.variableAccess()
		if err != nil {
			return nil, nil, err
		}
		readParameterList = append(readParameterList, expr)
	}

	if err = p.match(token.RParen); err != nil {
		return nil, nil, err
	}

	return file, readParameterList, nil
}

// readln-parameter-list = [ '(' ( file-variable | variable-access ) { ',' variable-access } ')' ] .
func (p *Parser) readLnParameterList() (*ast.Identifier, []ast.Expression, error) {
	var (
		err             error
		file            *ast.Identifier
		varAccess       ast.Expression
		readlnParamList []ast.Expression
	)

	if p.lAheadKind(1) == token.LParen {
		if err = p.match(token.LParen); err != nil {
			return nil, nil, err
		}

		sym := p.symTable.RetrieveSymbol(p.lAheadToken(1).Text)
		if sym != nil && sym.Type().Name() == "file" {
			file = &ast.Identifier{TokenKind: token.File, Name: sym.Name()}
		} else {
			varAccess, err = p.variableAccess()
			if err != nil {
				return nil, nil, err
			}
			readlnParamList = append(readlnParamList, varAccess)
		}

		for p.lAheadKind(1) == token.Comma {
			if err = p.consume(); err != nil {
				return nil, nil, err
			}

			varAccess, err = p.variableAccess()
			if err != nil {
				return nil, nil, err
			}
			readlnParamList = append(readlnParamList, varAccess)
		}

		if err = p.match(token.RParen); err != nil {
			return nil, nil, err
		}
	}

	file = &ast.Identifier{TokenKind: token.Output, Name: "output"}

	return file, readlnParamList, nil
}
