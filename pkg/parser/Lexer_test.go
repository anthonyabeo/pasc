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

	if token.Type != EOF {
		t.Errorf("lex.NextToken. Expected token type = %v, Got %v", EOF, GetTokenName(byte(token.Type)))
	}
}
