package parser

import (
	"fmt"
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
	lex.Consume()

	return lex
}

// Consume read the the current byte in the input and advances the pointer to the next byte
func (lex *Lexer) Consume() {
	if lex.curReadPos >= len(lex.input) {
		lex.curChar = byte(EOF)
	} else {
		lex.curChar = lex.input[lex.curReadPos]
	}

	lex.curCharPos = lex.curReadPos
	lex.curReadPos++
}

// NextToken constructs and returns the next token in the input stream
func (lex *Lexer) NextToken() (Token, error) {
	for lex.curChar != byte(EOF) {
		switch lex.curChar {
		case ' ', '\t', '\n', '\r':
			lex.consumeWhiteSpace()
			continue
		case '(':
			lex.Consume()
			return Token{Type: LParen, Text: "("}, nil
		case ')':
			lex.Consume()
			return Token{Type: RParen, Text: ")"}, nil
		case ';':
			lex.Consume()
			return Token{Type: SemiColon, Text: ";"}, nil
		case '\'':
			lex.Consume()
			tok := Token{Type: StrLiteral, Text: lex.readStringLiteral()}
			lex.Consume()

			return tok, nil
		case '.':
			lex.Consume()
			return Token{Type: Period, Text: "."}, nil
		default:
			if lex.curCharIsLetter() {
				name := lex.readName()

				return Token{Type: lex.getTypeOfName(name), Text: name}, nil
			}

			return Token{}, fmt.Errorf("invalid character: %v", lex.curChar)
		}
	}

	return Token{Text: "<EOF>", Type: EOF}, nil
}

func (lex *Lexer) getTypeOfName(name string) TokenType {
	if val, ok := Keywords[name]; ok {
		return val
	}

	return Identifier
}

func (lex *Lexer) readName() string {
	pos := lex.curCharPos
	for lex.curCharIsLetter() || lex.isDigit() {
		lex.Consume()
	}

	return lex.input[pos:lex.curCharPos]
}

func (lex *Lexer) readStringLiteral() string {
	pos := lex.curCharPos
	for lex.curChar != '\'' {
		lex.Consume()
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
		lex.Consume()
	}
}
