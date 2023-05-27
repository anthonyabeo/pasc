package ast

import "github.com/anthonyabeo/pasc/pkg/token"

// ForStatement ...
type ForStatement struct {
	Token                 token.Token
	CtrlID                *Identifier
	InitValue, FinalValue Expression
	Body                  Statement
	Direction             token.Kind
	Label                 string
}

// TokenLiteral returns the text value this node's token.
func (f *ForStatement) TokenLiteral() string { return f.Token.Text }

// TokenKind returns this node's token's kind
func (f *ForStatement) TokenKind() token.Kind { return f.Token.Kind }

// StatNode ...
func (f *ForStatement) StatNode() string {
	return ""
}

func (f *ForStatement) String() string {
	return ""
}

func (f *ForStatement) SetLabel(l string) {
	f.Label = l
}
