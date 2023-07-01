package ast

// Visitor ...
type Visitor interface {
	VisitIdentifier(*Identifier)
	VisitUIntLiteral(*UIntegerLiteral)
	VisitAssignStmt(*AssignStatement)
	VisitIndexedVariable(*IndexedVariable)
	VisitBinaryExpr(*BinaryExpression)
	VisitUnaryExpr(*UnaryExpression)
	VisitBoolLiteral(*BoolLiteral)
	VisitURealLiteral(*URealLiteral)
}
