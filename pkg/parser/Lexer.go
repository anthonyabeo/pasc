package parser

import (
	"fmt"
	"path/filepath"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// Lexer define a type that performs lexical analysis on the input stream.
//
// It is an implementation of an LL(1) Recursive Descent Lexer.
type Lexer struct {
	// immutable state
	file *token.SourceFile // source file handle
	dir  string            // directory portion of file.Name()
	src  []byte            // source

	// scanning state
	ch         rune // current character
	offset     int  // character offset
	rdOffset   int  // reading offset (position after current character)
	lineOffset int  // current line offset
}

func (lex *Lexer) Init(file *token.SourceFile, src []byte /*mode Mode*/) {
	// Explicitly initialize all fields since a scanner may be reused.
	if file.Size() != len(src) {
		panic(fmt.Sprintf("file size (%d) does not match src len (%d)", file.Size(), len(src)))
	}
	lex.file = file
	lex.dir, _ = filepath.Split(file.Name())
	lex.src = src

	lex.ch = ' '
	lex.offset = 0
	lex.rdOffset = 0
	lex.lineOffset = 0

	lex.consume()
}

const eof = -1 // end of file

func (lex *Lexer) consume() {
	if lex.rdOffset < len(lex.src) {
		lex.offset = lex.rdOffset
		if lex.ch == '\n' {
			lex.lineOffset = lex.offset
			lex.file.AddLine(lex.offset)
		}
		r, w := rune(lex.src[lex.rdOffset]), 1
		if r == 0 {
			panic(fmt.Sprintf("%s\n\tillegal character NUL",
				lex.file.Position(token.Pos(lex.offset))))
		}

		lex.rdOffset += w
		lex.ch = r
	} else {
		lex.offset = len(lex.src)
		if lex.ch == '\n' {
			lex.lineOffset = lex.offset
			lex.file.AddLine(lex.offset)
		}
		lex.ch = eof
	}
}

// NextToken constructs and returns the next token in the input stream
func (lex *Lexer) NextToken() (token.Token, error) {
	lex.consumeWhiteSpace()

	// current token start
	pos := lex.file.Position(lex.file.Pos(lex.offset))

	for lex.ch != eof {
		switch lex.ch {
		case '(':
			lex.consume()
			return token.Token{Kind: token.LParen, Text: "(", Pos: &pos}, nil
		case ')':
			lex.consume()
			return token.Token{Kind: token.RParen, Text: ")", Pos: &pos}, nil
		case ';':
			lex.consume()
			return token.Token{Kind: token.SemiColon, Text: ";", Pos: &pos}, nil
		case '\'':
			lex.consume()
			tok := token.NewToken(token.StrLiteral, lex.readStringLiteral(), &pos)
			lex.consume()

			return tok, nil
		case '.':
			lex.consume()
			if lex.ch == '.' {
				lex.consume()
				return token.Token{Kind: token.Range, Text: "..", Pos: &pos}, nil
			}

			return token.Token{Kind: token.Period, Text: ".", Pos: &pos}, nil
		case '[':
			lex.consume()
			return token.Token{Kind: token.LSqBrace, Text: "[", Pos: &pos}, nil
		case ']':
			lex.consume()
			return token.Token{Kind: token.RSqBrace, Text: "]", Pos: &pos}, nil
		case '+':
			lex.consume()
			return token.Token{Kind: token.Plus, Text: "+", Pos: &pos}, nil
		case '-':
			lex.consume()
			return token.Token{Kind: token.Minus, Text: "-", Pos: &pos}, nil
		case '*':
			lex.consume()
			return token.Token{Kind: token.Star, Text: "*", Pos: &pos}, nil
		case '/':
			lex.consume()
			return token.Token{Kind: token.FwdSlash, Text: "/", Pos: &pos}, nil
		case '=':
			lex.consume()
			return token.Token{Kind: token.Equal, Text: "=", Pos: &pos}, nil
		case '<':
			lex.consume()
			if lex.ch == '>' {
				lex.consume()
				return token.Token{Kind: token.NotEqual, Text: "<>", Pos: &pos}, nil
			}

			if lex.ch == '=' {
				lex.consume()
				return token.Token{Kind: token.LessThanOrEqual, Text: "<=", Pos: &pos}, nil
			}

			return token.Token{Kind: token.LessThan, Text: "<", Pos: &pos}, nil
		case '>':
			lex.consume()
			if lex.ch == '=' {
				lex.consume()
				return token.Token{Kind: token.GreaterThanOrEqual, Text: ">=", Pos: &pos}, nil
			}

			return token.Token{Kind: token.GreaterThan, Text: ">", Pos: &pos}, nil
		case ',':
			lex.consume()
			return token.Token{Kind: token.Comma, Text: ",", Pos: &pos}, nil
		case '^':
			lex.consume()
			return token.Token{Kind: token.Caret, Text: "^", Pos: &pos}, nil
		case ':':
			lex.consume()
			if lex.ch == '=' {
				lex.consume()
				return token.Token{Kind: token.Initialize, Text: ":=", Pos: &pos}, nil
			}

			return token.Token{Kind: token.Colon, Text: ":", Pos: &pos}, nil
		default:
			if lex.isLetter() {
				name := lex.readName()
				return token.Token{Kind: token.Lookup(name), Text: name, Pos: &pos}, nil
			}

			if lex.isDigit() {
				uNum := lex.readUnsignedNumber()
				if strings.Contains(uNum, "e") || strings.Contains(uNum, ".") {
					return token.Token{Kind: token.URealLiteral, Text: uNum, Pos: &pos}, nil
				}

				return token.Token{Kind: token.UIntLiteral, Text: uNum, Pos: &pos}, nil
			}

			return token.Token{Kind: token.Illegal, Text: string(lex.ch)},
				fmt.Errorf("invalid character: %v at %v", string(lex.ch), pos)
		}
	}

	return token.Token{Text: "<EOF>", Kind: token.EOF}, nil
}

func (lex *Lexer) readName() string {
	pos := lex.offset
	for lex.isLetter() || lex.isDigit() {
		lex.consume()
	}

	return string(lex.src[pos:lex.offset])
}

func (lex *Lexer) readStringLiteral() string {
	pos := lex.offset
	for lex.ch != '\'' {
		lex.consume()
	}

	return string(lex.src[pos:lex.offset])
}

func (lex *Lexer) readIntLiteral() string {
	pos := lex.offset
	for lex.isDigit() {
		lex.consume()
	}

	return string(lex.src[pos:lex.offset])
}

func (lex *Lexer) readUnsignedNumber() string {
	pos := lex.offset
	for lex.isDigit() {
		lex.consume()
	}

	if lex.src[lex.offset] == '.' && lex.src[lex.rdOffset] == '.' {
		return string(lex.src[pos:lex.offset])
	}

	// Unsigned real
	if lex.ch == '.' {
		lex.consume()

		// fractional-part
		for lex.isDigit() {
			lex.consume()
		}

		if lex.ch == 'e' {
			lex.consume()
		}

		if lex.ch == '+' || lex.ch == '-' {
			lex.consume()
		}

		for lex.isDigit() {
			lex.consume()
		}
	}

	if lex.ch == 'e' {
		lex.consume()
	}

	if lex.ch == '+' || lex.ch == '-' {
		lex.consume()
	}

	for lex.isDigit() {
		lex.consume()
	}

	return string(lex.src[pos:lex.offset])
}

func (lex *Lexer) isDigit() bool {
	return '0' <= lex.ch && lex.ch <= '9'
}

func (lex *Lexer) isLetter() bool {
	return 'a' <= lex.ch && lex.ch <= 'z' ||
		'A' <= lex.ch && lex.ch <= 'Z'
}

func (lex *Lexer) consumeWhiteSpace() {
	for lex.ch == ' ' || lex.ch == '\t' || lex.ch == '\n' || lex.ch == '\r' ||
		(lex.ch == '(' && lex.src[lex.rdOffset] == '*') || lex.ch == '{' {

		if (lex.ch == '(' && lex.src[lex.rdOffset] == '*') || lex.ch == '{' {
			for {
				if lex.ch == '*' && lex.src[lex.rdOffset] == ')' {
					lex.consume()
					lex.consume()
					break
				}

				if lex.ch == '}' {
					lex.consume()
					break
				}

				lex.consume()
			}
		}

		lex.consume()
	}
}
