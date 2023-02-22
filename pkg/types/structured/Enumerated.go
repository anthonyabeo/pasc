package structured

import "github.com/anthonyabeo/pasc/pkg/ast"

// Enumerated denoted an enumerated type
type Enumerated struct {
	List []*ast.Identifier
}

// GetName ...
func (e *Enumerated) GetName() string {
	return "enumerated-type"
}
