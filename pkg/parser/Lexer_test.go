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

	if tok.Kind != token.EOF {
		t.Errorf("lex.NextToken. Expected token type = %v, Got %v", token.EOF, token.GetTokenName(tok.Kind))
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
		expType token.Kind
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

		if tok.Kind != test.expType {
			t.Errorf("lex.NextToken. Expected token type = %v, Got %v",
				test.expText, token.GetTokenName(tok.Kind))
		}
	}
}

func TestTokenizeProgramWithSimpleArithmeticStatements(t *testing.T) {
	input := `
	program HelloWorld;
	var 
		a, b, sum : integer;

	begin
		a := 1;
		b := 2;
		sum := a + b;

		writeln(sum);
	end.
`
	lex := NewLexer(input)

	tests := []struct {
		expKind token.Kind
		expText string
	}{
		{token.Program, "program"},
		{token.Identifier, "HelloWorld"},
		{token.SemiColon, ";"},
		{token.Var, "var"},
		{token.Identifier, "a"},
		{token.Comma, ","},
		{token.Identifier, "b"},
		{token.Comma, ","},
		{token.Identifier, "sum"},
		{token.Colon, ":"},
		{token.Integer, "integer"},
		{token.SemiColon, ";"},
		{token.Begin, "begin"},
		{token.Identifier, "a"},
		{token.Initialize, ":="},
		{token.IntLiteral, "1"},
		{token.SemiColon, ";"},
		{token.Identifier, "b"},
		{token.Initialize, ":="},
		{token.IntLiteral, "2"},
		{token.SemiColon, ";"},
		{token.Identifier, "sum"},
		{token.Initialize, ":="},
		{token.Identifier, "a"},
		{token.Plus, "+"},
		{token.Identifier, "b"},
		{token.SemiColon, ";"},
		{token.Identifier, "writeln"},
		{token.LParen, "("},
		{token.Identifier, "sum"},
		{token.RParen, ")"},
		{token.SemiColon, ";"},
		{token.End, "end"},
		{token.Period, "."},
	}

	for _, tt := range tests {
		tok, err := lex.NextToken()
		if err != nil {
			t.Errorf(fmt.Sprintf("lex.NextToken. %s", err.Error()))
		}

		if tok.Text != tt.expText {
			t.Errorf("lex.NextToken. Expected token text = %v, Got %v", tt.expText, tok.Text)
		}

		if tok.Kind != tt.expKind {
			t.Errorf("lex.NextToken. Expected token type = %v, Got %v",
				tt.expText, token.GetTokenName(tok.Kind))
		}
	}
}
