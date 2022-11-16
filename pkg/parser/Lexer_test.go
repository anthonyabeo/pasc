package parser

import "testing"

func TestLexingEmptyString(t *testing.T) {
	input := ""

	lex := Lexer{input: input}
	token := lex.NextToken()

	if token.Text != "<EOF>" {
		t.Errorf("lex.NextToken. Expected token text = <EOF>, Got %v", token.Text)
	}

	if token.Type != EOF {
		t.Errorf("lex.NextToken. Expected token type = %v, Got %v", EOF, token.GetTokenName())
	}
}
