package ast

type ProcedureStatement interface {
	Statement
	GetName() string
	GetParamList() []Expression
}
