package parser

import (
	"fmt"
	"testing"
)

func TestLexingEmptyString(t *testing.T) {
	input := ""

	lex := Lexer{input: input}
	token, err := lex.NextToken()
	if err != nil {
		t.Errorf(fmt.Sprintf("lex.NextToken. %s", err.Error()))
	}

	if token.Text != "<EOF>" {
		t.Errorf("lex.NextToken. Expected token text = <EOF>, Got %v", token.Text)
	}

	if token.Type != Eof {
		t.Errorf("lex.NextToken. Expected token type = %v, Got %v", Eof, GetTokenName(byte(token.Type)))
	}
}

func TestLexingHelloWorldProgram(t *testing.T) {
	input := `
		program HelloWorld;
		begin
			writeln('Hello, World!');
		end.
	`

	tests := []struct {
		expType TokenType
		expText string
	}{
		{Program, "program"},
		{Identifier, "HelloWorld"},
		{SemiColon, ";"},
		{Begin, "begin"},
		{Identifier, "writeln"},
		{LParen, "("},
		{StrLiteral, "Hello, World!"},
		{RParen, ")"},
		{SemiColon, ";"},
		{End, "end"},
		{Period, "."},
	}

	lex := NewLexer(input)

	for _, test := range tests {
		token, err := lex.NextToken()
		if err != nil {
			t.Errorf(fmt.Sprintf("lex.NextToken. %s", err.Error()))
		}

		if token.Text != test.expText {
			t.Errorf("lex.NextToken. Expected token text = %v, Got %v", test.expText, token.Text)
		}

		if token.Type != test.expType {
			t.Errorf("lex.NextToken. Expected token type = %v, Got %v", test.expText, GetTokenName(byte(token.Type)))
		}
	}
}
