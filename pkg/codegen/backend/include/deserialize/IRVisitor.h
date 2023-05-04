#ifndef IR_VISITOR_H
#define IR_VISITOR_H

#include "llvm/IR/Value.h"

#include "Block.h"
#include "Expr.h"
#include "Statement.h"

class IRVisitor {
public:
  virtual llvm::Value *codegen(const VariableID &) = 0;
  virtual llvm::Value *codegen(const IdentifierExpr &) = 0;
  virtual llvm::Value *codegen(const UIntegerLiteral &) = 0;
  virtual llvm::Value *codegen(const BinaryExpression &) = 0;
  virtual llvm::Value *codegen(const FunctionCall &) = 0;
  virtual llvm::Value *codegen(const WriteParameter &) = 0;

  virtual llvm::Value *codegen(const AssignStmt &) = 0;
  virtual llvm::Value *codegen(const ReturnStatement &) = 0;
  virtual llvm::Value *codegen(const IfStatement &) = 0;
  virtual llvm::Value *codegen(const ProcedureStmt &) = 0;
  virtual llvm::Value *codegen(const Writeln &) = 0;
};

#endif // IR_VISITOR_H