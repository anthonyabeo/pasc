#ifndef IR_VISITOR_H
#define IR_VISITOR_H

#include "llvm/IR/Value.h"

#include "Block.h"
#include "Callable.h"
#include "Expr.h"
#include "IRVisitor.h"
#include "Statement.h"
#include "Type.h"

class IRVisitor {
public:
  virtual llvm::Value *codegen(const VariableID &) = 0;
  virtual llvm::Value *codegen(const IdentifierExpr &) = 0;
  virtual llvm::Value *codegen(const UIntegerLiteral &) = 0;
  virtual llvm::Value *codegen(const BinaryExpression &) = 0;
  virtual llvm::Value *codegen(const FunctionCall &) = 0;
  virtual llvm::Value *codegen(const WriteParameter &) = 0;
  virtual llvm::Value *codegen(const URealLiteral &) = 0;
  virtual llvm::Value *codegen(const CharString &) = 0;
  virtual llvm::Value *codegen(const UnaryExpression &) = 0;
  virtual llvm::Value *codegen(const BoolExpr &) = 0;
  virtual llvm::Value *codegen(const IndexedVariable &) = 0;
  virtual llvm::Value *codegen(const IndexedVarExpr &) = 0;
  virtual llvm::Value *codegen(const FieldDesignator &) = 0;
  virtual llvm::Value *codegen(const FieldDesigExpr &) = 0;
  virtual llvm::Value *codegen(const Range &) = 0;
  virtual llvm::Value *codegen(const Nil &) = 0;

  virtual llvm::Value *codegen(const AssignStmt &) = 0;
  virtual llvm::Value *codegen(const ReturnStatement &) = 0;
  virtual llvm::Value *codegen(const IfStatement &) = 0;
  virtual llvm::Value *codegen(const ProcedureStmt &) = 0;
  virtual llvm::Value *codegen(const Writeln &) = 0;
  virtual llvm::Value *codegen(const Write &) = 0;
  virtual llvm::Value *codegen(const FunctionDeclaration &) = 0;
  virtual llvm::Value *codegen(const ProcedureDeclaration &) = 0;
  virtual llvm::Value *codegen(const WhileStatement &) = 0;
  virtual llvm::Value *codegen(const CompoundStatement &) = 0;
  virtual llvm::Value *codegen(const RepeatStatement &) = 0;
  virtual llvm::Value *codegen(const ForStatement &) = 0;
  virtual llvm::Value *codegen(const GotoStatement &) = 0;
  virtual llvm::Value *codegen(const CaseStatement &) = 0;
  virtual llvm::Value *codegen(const WithStatement &) = 0;

  virtual std::vector<llvm::Type*> codegen(const VariableParam &) = 0;
  virtual std::vector<llvm::Type*> codegen(const ValueParam &) = 0;
  virtual std::vector<llvm::Type*> codegen(const FuncHeading &) = 0;
  virtual std::vector<llvm::Type*> codegen(const ProcHeading &) = 0;

  virtual llvm::Type *codegen(const IntegerType &) = 0;
  virtual llvm::Type *codegen(const BoolType &) = 0;
  virtual llvm::Type *codegen(const VoidType &) = 0;
  virtual llvm::Type *codegen(const RealType &) = 0;
  virtual llvm::Type *codegen(const StringType &) = 0;
  virtual llvm::Type *codegen(const EnumType &) = 0;
  virtual llvm::Type *codegen(const SubRangeType &) = 0;
  virtual llvm::Type *codegen(const ArrayType &) = 0;
  virtual llvm::Type *codegen(const CharType &) = 0;
  virtual llvm::Type *codegen(const RecordType &) = 0;
};

#endif // IR_VISITOR_H