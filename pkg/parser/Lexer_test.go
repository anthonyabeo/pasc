package parser

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/pasc/pkg/token"
)

func TestLexingEmptyString(t *testing.T) {
	input := ""

	lex := Lexer{input: input}
	tok, err := lex.NextToken()
	if err != nil {
		t.Errorf(fmt.Sprintf("lex.NextToken. %s", err.Error()))
	}

	if tok.Text != "<EOF>" {
		t.Errorf("lex.NextToken. Expected token text = <EOF>, Got %v", tok.Text)
	}

	if tok.Type != token.EOF {
		t.Errorf("lex.NextToken. Expected token type = %v, Got %v", token.EOF, token.GetTokenName(tok.Type))
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
		expType token.Type
		expText string
	}{
		{token.Program, "program"},
		{token.Identifier, "HelloWorld"},
		{token.SemiColon, ";"},
		{token.Begin, "begin"},
		{token.Identifier, "writeln"},
		{token.LParen, "("},
		{token.StrLiteral, "Hello, World!"},
		{token.RParen, ")"},
		{token.SemiColon, ";"},
		{token.End, "end"},
		{token.Period, "."},
	}

	lex := NewLexer(input)

	for _, test := range tests {
		tok, err := lex.NextToken()
		if err != nil {
			t.Errorf(fmt.Sprintf("lex.NextToken. %s", err.Error()))
		}

		if tok.Text != test.expText {
			t.Errorf("lex.NextToken. Expected token text = %v, Got %v", test.expText, tok.Text)
		}

		if tok.Type != test.expType {
			t.Errorf("lex.NextToken. Expected token type = %v, Got %v",
				test.expText, token.GetTokenName(tok.Type))
		}
	}
}
