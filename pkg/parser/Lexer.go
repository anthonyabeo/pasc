package parser

import (
	"fmt"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// Lexer define a type that performs lexical analysis on the input stream.
//
// It is an implementation of an LL(1) Recursive Descent Lexer.
type Lexer struct {
	input                  string
	curChar                byte
	curCharPos, curReadPos int
}

// NewLexer create, initializes and returns a new lexer entity
func NewLexer(input string) Lexer {
	lex := Lexer{input: input}
	lex.consume()

	return lex
}

// Consume read the the current byte in the input and advances the pointer to the next byte
func (lex *Lexer) consume() {
	if lex.curReadPos >= len(lex.input) {
		lex.curChar = byte(token.EOF)
	} else {
		lex.curChar = lex.input[lex.curReadPos]
	}

	lex.curCharPos = lex.curReadPos
	lex.curReadPos++
}

// NextToken constructs and returns the next token in the input stream
func (lex *Lexer) NextToken() (token.Token, error) {
	for lex.curChar != byte(token.EOF) {
		switch lex.curChar {
		case ' ', '\t', '\n', '\r':
			lex.consumeWhiteSpace()
			continue
		case '(':
			lex.consume()
			return token.Token{Type: token.LParen, Text: "("}, nil
		case ')':
			lex.consume()
			return token.Token{Type: token.RParen, Text: ")"}, nil
		case ';':
			lex.consume()
			return token.Token{Type: token.SemiColon, Text: ";"}, nil
		case '\'':
			lex.consume()
			tok := token.Token{Type: token.StrLiteral, Text: lex.readStringLiteral()}
			lex.consume()

			return tok, nil
		case '.':
			lex.consume()
			return token.Token{Type: token.Period, Text: "."}, nil
		default:
			if lex.curCharIsLetter() {
				name := lex.readName()

				return token.Token{Type: lex.getTypeOfName(name), Text: name}, nil
			}

			return token.Token{}, fmt.Errorf("invalid character: %v", lex.curChar)
		}
	}

	return token.Token{Text: "<EOF>", Type: token.EOF}, nil
}

func (lex *Lexer) getTypeOfName(name string) token.Type {
	if val, ok := token.Keywords[name]; ok {
		return val
	}

	return token.Identifier
}

func (lex *Lexer) readName() string {
	pos := lex.curCharPos
	for lex.curCharIsLetter() || lex.isDigit() {
		lex.consume()
	}

	return lex.input[pos:lex.curCharPos]
}

func (lex *Lexer) readStringLiteral() string {
	pos := lex.curCharPos
	for lex.curChar != '\'' {
		lex.consume()
	}

	return lex.input[pos:lex.curCharPos]
}

func (lex *Lexer) isDigit() bool {
	return '0' <= lex.curChar && lex.curChar <= '9'
}

func (lex *Lexer) curCharIsLetter() bool {
	return 'a' <= lex.curChar && lex.curChar <= 'z' ||
		'A' <= lex.curChar && lex.curChar <= 'Z'
}

func (lex *Lexer) consumeWhiteSpace() {
	for lex.curChar == ' ' || lex.curChar == '\t' || lex.curChar == '\n' || lex.curChar == '\r' {
		lex.consume()
	}
}
