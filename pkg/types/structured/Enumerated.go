package structured

import (
	"fmt"
	"strings"

	"github.com/anthonyabeo/pasc/pkg/ast"
	"github.com/anthonyabeo/pasc/pkg/types"
)

// Enumerated denoted an enumerated type
type Enumerated struct {
	List []*ast.Identifier
}

// Name ...
func (e *Enumerated) Name() string {
	return "enum"
}

func (e *Enumerated) String() string {
	var elems []string
	for _, elem := range e.List {
		elems = append(elems, elem.Name)
	}

	return fmt.Sprintf("(%s)", strings.Join(elems, ", "))
}

func (e *Enumerated) Ord() {}

func (e *Enumerated) Underlying() types.Type {
	return e
}
