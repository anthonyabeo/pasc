package parser

// Lexer ...
type Lexer struct {
	input                  string
	curChar                byte
	curCharPos, curReadPos int
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
func (lex *Lexer) NextToken() Token {
	for lex.curChar != byte(EOF) {
		switch lex.curChar {

		}
	}

	return Token{Text: "<EOF>", Type: EOF}
}
