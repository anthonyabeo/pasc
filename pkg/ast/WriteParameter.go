package ast

import "github.com/anthonyabeo/pasc/pkg/types"

// WriteParameter is the node that represents a single parameter to be passed to the write[ln] procedure
type WriteParameter struct {
	E          Expression
	TotalWidth Expression
	FracDigits Expression
	EType      types.Type
}

func (w *WriteParameter) expr() {}

func (w *WriteParameter) Accept(v Visitor) {
	v.VisitWriteParameter(w)
}

func (w *WriteParameter) Type() types.Type {
	return w.EType
}

func (w *WriteParameter) String() string {
	return w.E.String()
}
