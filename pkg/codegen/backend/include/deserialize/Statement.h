#ifndef STATEMENT_H
#define STATEMENT_H

 #include "llvm/IR/Value.h"

 #include "Expr.h"

 class IRVisitor;

struct Statement {
  virtual ~Statement() = default;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
};

std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &);

struct AssignStmt : public Statement {
  std::unique_ptr<IdentifierIR> variable;
  std::unique_ptr<Expr> value;

  explicit AssignStmt(const Pasc::AssignStmt &stmt);
  llvm::Value *codegen(IRVisitor &) override;
};

struct ProcedureStatement : public Statement {
  std::unique_ptr<IdentifierIR> name;
  std::vector<std::unique_ptr<Expr>> params;

  explicit ProcedureStatement(const Pasc::ProcedureStmt &stmt);
  llvm::Value *codegen(IRVisitor &) override;
};

#endif // STATEMENT_H