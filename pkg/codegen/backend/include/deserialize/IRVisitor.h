#ifndef IR_VISITOR_H
#define IR_VISITOR_H

#include "llvm/IR/Value.h"

#include "Block.h"
#include "Expr.h"
#include "Statement.h"

class IRVisitor {
public:
  virtual llvm::Value *codegen(const IdentifierIR &) = 0;
  virtual llvm::Value *codegen(const UIntegerLiteral &) = 0;
  virtual llvm::Value *codegen(const BinaryExpression &) = 0;

  virtual llvm::Value *codegen(const AssignStmt &) = 0;
  virtual llvm::Value *codegen(const ProcedureStatement &) = 0;
  virtual llvm::Value *codegen(const IfStatement &) = 0;
};

#endif // IR_VISITOR_H