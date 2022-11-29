package dtype

import "github.com/anthonyabeo/pasc/pkg/token"

// Integer is an integer data type
type Integer struct {
	Token token.Token
}

// NewInteger ...
func NewInteger(tt token.Token) *Integer {
	return &Integer{Token: tt}
}

// GetName ...
func (i *Integer) GetName() string {
	return i.Token.Text
}
