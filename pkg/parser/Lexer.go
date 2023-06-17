package parser

import (
	"fmt"
	"strings"

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

// Consume read the current byte in the input and advances the pointer to the next byte
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
	lex.consumeWhiteSpace()

	for lex.curChar != byte(token.EOF) {
		switch lex.curChar {
		case '(':
			lex.consume()
			return token.Token{Kind: token.LParen, Text: "("}, nil
		case ')':
			lex.consume()
			return token.Token{Kind: token.RParen, Text: ")"}, nil
		case ';':
			lex.consume()
			return token.Token{Kind: token.SemiColon, Text: ";"}, nil
		case '\'':
			lex.consume()
			tok := token.Token{Kind: token.StrLiteral, Text: lex.readStringLiteral()}
			lex.consume()

			return tok, nil
		case '.':
			lex.consume()
			if lex.curChar == '.' {
				lex.consume()
				return token.Token{Kind: token.Range, Text: ".."}, nil
			}

			return token.Token{Kind: token.Period, Text: "."}, nil
		case '[':
			lex.consume()
			return token.Token{Kind: token.LSqBrace, Text: "["}, nil
		case ']':
			lex.consume()
			return token.Token{Kind: token.RSqBrace, Text: "]"}, nil
		case '+':
			lex.consume()
			return token.Token{Kind: token.Plus, Text: "+"}, nil
		case '-':
			lex.consume()
			return token.Token{Kind: token.Minus, Text: "-"}, nil
		case '*':
			lex.consume()
			return token.Token{Kind: token.Star, Text: "*"}, nil
		case '/':
			lex.consume()
			return token.Token{Kind: token.FwdSlash, Text: "/"}, nil
		case '=':
			lex.consume()
			return token.Token{Kind: token.Equal, Text: "="}, nil
		case '<':
			lex.consume()
			if lex.curChar == '>' {
				lex.consume()
				return token.Token{Kind: token.LessThanGreaterThan, Text: "<>"}, nil
			}

			if lex.curChar == '=' {
				lex.consume()
				return token.Token{Kind: token.LessThanOrEqual, Text: "<="}, nil
			}

			return token.Token{Kind: token.LessThan, Text: "<"}, nil
		case '>':
			lex.consume()
			if lex.curChar == '=' {
				lex.consume()
				return token.Token{Kind: token.GreaterThanOrEqual, Text: ">="}, nil
			}

			return token.Token{Kind: token.GreaterThan, Text: ">"}, nil
		case ',':
			lex.consume()
			return token.Token{Kind: token.Comma, Text: ","}, nil
		case '^':
			lex.consume()
			return token.Token{Kind: token.Caret, Text: "^"}, nil
		case ':':
			lex.consume()
			if lex.curChar == '=' {
				lex.consume()
				return token.Token{Kind: token.Initialize, Text: ":="}, nil
			}

			return token.Token{Kind: token.Colon, Text: ":"}, nil
		default:
			if lex.isLetter() {
				name := lex.readName()
				return token.Token{Kind: lex.getTypeOfName(name), Text: name}, nil
			}

			if lex.isDigit() {
				uNum := lex.readUnsignedNumber()
				if strings.Contains(uNum, "e") || strings.Contains(uNum, ".") {
					return token.Token{Kind: token.URealLiteral, Text: uNum}, nil
				}

				return token.Token{Kind: token.UIntLiteral, Text: uNum}, nil
			}

			return token.Token{}, fmt.Errorf("invalid character: %v", string(lex.curChar))
		}
	}

	return token.Token{Text: "<EOF>", Kind: token.EOF}, nil
}

func (lex *Lexer) getTypeOfName(name string) token.Kind {
	if val, ok := token.Keywords[name]; ok {
		return val
	}

	return token.Identifier
}

func (lex *Lexer) readName() string {
	pos := lex.curCharPos
	for lex.isLetter() || lex.isDigit() {
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

func (lex *Lexer) readIntLiteral() string {
	pos := lex.curCharPos
	for lex.isDigit() {
		lex.consume()
	}

	return lex.input[pos:lex.curCharPos]
}

func (lex *Lexer) readUnsignedNumber() string {
	pos := lex.curCharPos
	for lex.isDigit() {
		lex.consume()
	}

	if lex.input[lex.curCharPos] == '.' && lex.input[lex.curReadPos] == '.' {
		return lex.input[pos:lex.curCharPos]
	}

	// Unsigned real
	if lex.curChar == '.' {
		lex.consume()

		// fractional-part
		for lex.isDigit() {
			lex.consume()
		}

		if lex.curChar == 'e' {
			lex.consume()
		}

		if lex.curChar == '+' || lex.curChar == '-' {
			lex.consume()
		}

		for lex.isDigit() {
			lex.consume()
		}
	}

	if lex.curChar == 'e' {
		lex.consume()
	}

	if lex.curChar == '+' || lex.curChar == '-' {
		lex.consume()
	}

	for lex.isDigit() {
		lex.consume()
	}

	return lex.input[pos:lex.curCharPos]
}

func (lex *Lexer) isDigit() bool {
	return '0' <= lex.curChar && lex.curChar <= '9'
}

func (lex *Lexer) isLetter() bool {
	return 'a' <= lex.curChar && lex.curChar <= 'z' ||
		'A' <= lex.curChar && lex.curChar <= 'Z'
}

func (lex *Lexer) consumeWhiteSpace() {
	for lex.curChar == ' ' || lex.curChar == '\t' || lex.curChar == '\n' || lex.curChar == '\r' ||
		(lex.curChar == '(' && lex.input[lex.curReadPos] == '*') || lex.curChar == '{' {

		if (lex.curChar == '(' && lex.input[lex.curReadPos] == '*') || lex.curChar == '{' {
			for {
				if lex.curChar == '*' && lex.input[lex.curReadPos] == ')' {
					lex.consume()
					lex.consume()
					break
				}

				if lex.curChar == '}' {
					lex.consume()
					break
				}

				lex.consume()
			}
		}

		lex.consume()
	}
}
