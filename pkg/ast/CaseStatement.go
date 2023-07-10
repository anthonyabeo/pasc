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
	Label string
}

func (c *CaseStatement) stmt() {}

func (c *CaseStatement) Accept(vst Visitor) error {
	return vst.VisitCaseStatement(c)
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

func (c *CaseStatement) SetLabel(l string) {
	c.Label = l
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

	return fmt.Sprintf(`%v:%v`, strings.Join(constList, ", "), c.Body)
}
