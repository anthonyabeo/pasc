package parser

import (
	"fmt"
	"testing"

	"github.com/anthonyabeo/pasc/pkg/token"
)

func TestLexingEmptyString(t *testing.T) {
	input := []byte("")

	fs := token.NewFileSet()
	file := fs.AddFile("test.go", -1, len(input))

	lex := Lexer{}
	lex.Init(file, input)

	tok, err := lex.NextToken()
	if err != nil {
		t.Errorf(fmt.Sprintf("lex.NextToken. %s", err.Error()))
	}

	if tok.Text != "<EOF>" {
		t.Errorf("lex.NextToken. Expected token text = <EOF>, Got %v", tok.Text)
	}

	if tok.Kind != token.EOF {
		t.Errorf("lex.NextToken. Expected token type = %v, Got %v", token.EOF, tok.Kind)
	}
}

func TestLexingHelloWorldProgram(t *testing.T) {
	input := []byte(`program HelloWorld;
begin
    writeln('Hello, World!');
end.
`)

	tests := []struct {
		expType token.Kind
		expText string
		expCol  int
		expLine int
	}{
		{token.Program, "program", 1, 1},
		{token.Identifier, "HelloWorld", 9, 1},
		{token.SemiColon, ";", 19, 1},
		{token.Begin, "begin", 1, 2},
		{token.Identifier, "writeln", 5, 3},
		{token.LParen, "(", 12, 3},
		{token.StrLiteral, "Hello, World!", 13, 3},
		{token.RParen, ")", 28, 3},
		{token.SemiColon, ";", 29, 3},
		{token.End, "end", 1, 4},
		{token.Period, ".", 4, 4},
	}

	fs := token.NewFileSet()
	file := fs.AddFile("test.go", -1, len(input))

	lex := Lexer{}
	lex.Init(file, input)

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
				test.expText, tok.Kind)
		}

		if tok.Pos.Line != test.expLine {
			t.Errorf("lex.NextToken. Expected line number (of token %s) to be %d, Got %d instead",
				tok.Text, test.expLine, tok.Pos.Line)
		}

		if tok.Pos.Column != test.expCol {
			t.Errorf("lex.NextToken. Expected column number (of token %s) to be %d, Got %d instead",
				tok.Text, test.expCol, tok.Pos.Column)
		}
	}
}

func TestTokenizeProgramWithSimpleArithmeticStatements(t *testing.T) {
	input := []byte(`program HelloWorld;
var
	a, b, sum : integer;

begin
	(* this is a comment *)
	a := 1;
	b := 2;
	sum := a + b;

	writeln(sum);
end.
`)
	fs := token.NewFileSet()
	file := fs.AddFile("test.go", -1, len(input))

	lex := Lexer{}
	lex.Init(file, input)

	tests := []struct {
		expKind token.Kind
		expText string
		expCol  int
		expLine int
	}{
		{token.Program, "program", 1, 1},
		{token.Identifier, "HelloWorld", 9, 1},
		{token.SemiColon, ";", 19, 1},
		{token.Var, "var", 2, 1},
		{token.Identifier, "a", 2, 3},
		{token.Comma, ",", 3, 3},
		{token.Identifier, "b", 5, 3},
		{token.Comma, ",", 6, 3},
		{token.Identifier, "sum", 8, 3},
		{token.Colon, ":", 10, 3},
		{token.Integer, "integer", 12, 3},
		{token.SemiColon, ";", 19, 1},
		{token.Begin, "begin", 1, 5},
		{token.Identifier, "a", 2, 7},
		{token.Initialize, ":=", 4, 7},
		{token.UIntLiteral, "1", 6, 7},
		{token.SemiColon, ";", 7, 7},
		{token.Identifier, "b", 2, 8},
		{token.Initialize, ":=", 4, 8},
		{token.UIntLiteral, "2", 6, 8},
		{token.SemiColon, ";", 7, 8},
		{token.Identifier, "sum", 2, 9},
		{token.Initialize, ":=", 6, 9},
		{token.Identifier, "a", 8, 9},
		{token.Plus, "+", 10, 9},
		{token.Identifier, "b", 12, 9},
		{token.SemiColon, ";", 13, 9},
		{token.Identifier, "writeln", 2, 11},
		{token.LParen, "(", 9, 11},
		{token.Identifier, "sum", 10, 11},
		{token.RParen, ")", 13, 11},
		{token.SemiColon, ";", 14, 11},
		{token.End, "end", 1, 12},
		{token.Period, ".", 4, 12},
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
				tt.expText, tok.Kind)
		}
	}
}

func TestOperators(t *testing.T) {
	input := []byte(`<> <= >=`)

	tests := []struct {
		expKind token.Kind
		expText string
	}{
		{token.NotEqual, "<>"},
		{token.LessThanOrEqual, "<="},
		{token.GreaterThanOrEqual, ">="},
	}

	fs := token.NewFileSet()
	file := fs.AddFile("test.go", -1, len(input))

	lex := Lexer{}
	lex.Init(file, input)

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
				tt.expText, tok.Kind)
		}
	}
}

func TestTokenizingUnsignedReal(t *testing.T) {
	input := []byte(`
		854.32
		6.023e-23
		123e+4
		1.2
		0.000232
		(* this is a comment *)
		0.000232e-4837294
		3.142
		1.2e4938
	`)

	tests := []struct {
		expKind token.Kind
		expText string
	}{
		{token.URealLiteral, "854.32"},
		{token.URealLiteral, "6.023e-23"},
		{token.URealLiteral, "123e+4"},
		{token.URealLiteral, "1.2"},
		{token.URealLiteral, "0.000232"},
		{token.URealLiteral, "0.000232e-4837294"},
		{token.URealLiteral, "3.142"},
		{token.URealLiteral, "1.2e4938"},
	}

	fs := token.NewFileSet()
	file := fs.AddFile("test.go", -1, len(input))

	lex := Lexer{}
	lex.Init(file, input)

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
				tt.expText, tok.Kind)
		}
	}
}

func TestTokenizeSubRangeType(t *testing.T) {
	input := []byte(`
		(* this is a comment *)
		1900..1999
		red..green
		'0'..'9'
		-10..+10
		{ this is a comment }
	`)

	tests := []struct {
		expKind token.Kind
		expText string
	}{
		{token.UIntLiteral, "1900"},
		{token.Range, ".."},
		{token.UIntLiteral, "1999"},
		{token.Identifier, "red"},
		{token.Range, ".."},
		{token.Identifier, "green"},
		{token.StrLiteral, "0"},
		{token.Range, ".."},
		{token.StrLiteral, "9"},
		{token.Minus, "-"},
		{token.UIntLiteral, "10"},
		{token.Range, ".."},
		{token.Plus, "+"},
		{token.UIntLiteral, "10"},
	}

	fs := token.NewFileSet()
	file := fs.AddFile("test.go", -1, len(input))

	lex := Lexer{}
	lex.Init(file, input)

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
				tt.expText, tok.Kind)
		}
	}
}

func TestTokenizePointer(t *testing.T) {
	input := []byte(`
		p1^.mother := true
		{ this is a comment }
		p1^.sibling^.father^
	`)

	tests := []struct {
		expKind token.Kind
		expText string
	}{
		{token.Identifier, "p1"},
		{token.Caret, "^"},
		{token.Period, "."},
		{token.Identifier, "mother"},
		{token.Initialize, ":="},
		{token.True, "true"},

		{token.Identifier, "p1"},
		{token.Caret, "^"},
		{token.Period, "."},
		{token.Identifier, "sibling"},
		{token.Caret, "^"},
		{token.Period, "."},
		{token.Identifier, "father"},
		{token.Caret, "^"},
	}

	fs := token.NewFileSet()
	file := fs.AddFile("test.go", -1, len(input))

	lex := Lexer{}
	lex.Init(file, input)

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
				tt.expText, tok)
		}
	}
}
