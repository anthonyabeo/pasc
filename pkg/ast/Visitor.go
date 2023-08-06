package ast

// Visitor ...
type Visitor interface {
	VisitIdentifier(*Identifier) error
	VisitUIntLiteral(*UIntegerLiteral) error
	VisitAssignStmt(*AssignStatement) error
	VisitIndexedVariable(*IndexedVariable) error
	VisitBinaryExpr(*BinaryExpression) error
	VisitUnaryExpr(*UnaryExpression) error
	VisitBoolLiteral(*BoolLiteral) error
	VisitURealLiteral(*URealLiteral) error
	VisitForStatement(*ForStatement) error
	VisitIfStatement(*IfStatement) error
	VisitFuncDesignator(*FuncDesignator) error
	VisitGotoStatement(*GotoStatement) error
	VisitIdentifiedVariable(*IdentifiedVariable) error
	VisitWhileStatement(*WhileStatement) error
	VisitWithStatement(*WithStatement) error
	VisitRepeatStatement(*RepeatStatement) error
	VisitReturnStatement(*ReturnStatement) error
	VisitRange(*Range) error
	VisitFieldDesignator(*FieldDesignator) error
	VisitCompoundStatement(*CompoundStatement) error
	VisitStrLiteral(*StrLiteral) error
	VisitFuncDeclaration(*FuncDeclaration) error
	VisitProcedureDecl(*ProcedureDeclaration) error
	VisitProcedureStmt(*ProcedureStmt) error
	VisitRead(*Read) error
	VisitReadLn(*ReadLn) error
	VisitWrite(*Write) error
	VisitWriteln(*Writeln) error
	VisitNil(*NilValue) error
	VisitCaseStatement(*CaseStatement) error
	VisitWriteParameter(*WriteParameter) error
	VisitSetConstructor(*SetConstructor) error
	VisitValueParam(*ValueParam) error
	VisitVariableParam(*VariableParam) error
	VisitFuncHeading(*FuncHeading) error
	VisitProcedureHeading(*ProcedureHeading) error
	VisitVarDecl(*VarDeclaration) error
	VisitConstDef(*ConstDefinition) error
	VisitLabelDef(*LabelDefinition) error
	VisitTypeDef(*TypeDefinition) error
}
