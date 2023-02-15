package ast

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/token"
)

// CaseStatement models the AST node of a Case Statement
type CaseStatement struct {
	Token token.Token
	Index Expression
	List  []*CaseElement
}

// TokenLiteral returns the text value this node's token.
func (c *CaseStatement) TokenLiteral() string { return c.Token.Text }

// TokenKind returns this node's token's kind
func (c *CaseStatement) TokenKind() token.Kind { return c.Token.Kind }

// StatNode ...
func (c *CaseStatement) StatNode() string {
	var caseList []string
	for _, elem := range c.List {
		caseList = append(caseList, elem.String(), "\n")
	}

	return fmt.Sprintf(`
	case %v of
		%v
	end
	`, c.Index.String(), strings.Join(caseList, ", "))
}

func (c *CaseStatement) String() string {
	var caseList []string
	for _, elem := range c.List {
		caseList = append(caseList, elem.String(), "\n")
	}

	return fmt.Sprintf(`
	case %v of
		%v
	end
	`, c.Index.String(), strings.Join(caseList, ", "))
}

// CaseElement ...
type CaseElement struct {
	ConstList []Expression
	Body      Statement
}

func (c *CaseElement) String() string {
	var constList []string
	for _, c := range c.ConstList {
		constList = append(constList, c.String())
	}

	return fmt.Sprintf(`%v:%v`, strings.Join(constList, ", "), c.Body.StatNode())
}
