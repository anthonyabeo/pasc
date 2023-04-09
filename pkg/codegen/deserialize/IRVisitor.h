#ifndef IR_VISITOR
#define IR_VISITOR

#include "llvm/IR/Value.h"

#include "Block.h"
#include "Expr.h"
// #include "Statement.h"

class IRVisitor {
public:
  virtual llvm::Value *codegen(const Identifier &) = 0;
  virtual llvm::Value *codegen(const UIntegerLiteral &) = 0;

  virtual llvm::Value *codegen(const AssignStmt &) = 0;
  virtual llvm::Value *codegen(const ProcedureStatement &) = 0;
};

#endif // IR_VISITOR