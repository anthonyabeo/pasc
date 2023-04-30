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
  std::unique_ptr<Identifier> variable;
  std::unique_ptr<Expr> value;

  explicit AssignStmt(const Pasc::AssignStmt &stmt);
  llvm::Value *codegen(IRVisitor &) override;
};

struct ProcedureStatement : public Statement {
  std::unique_ptr<Identifier> name;
  std::vector<std::unique_ptr<Expr>> params;

  explicit ProcedureStatement(const Pasc::ProcedureStmt &stmt);
  llvm::Value *codegen(IRVisitor &) override;
};

struct IfStatement : public Statement {
  std::unique_ptr<Expr> cond;
  std::unique_ptr<Statement> true_path;
  std::unique_ptr<Statement> else_path;

  explicit IfStatement(const Pasc::IfStmt&);
  llvm::Value *codegen(IRVisitor&) override;
};

#endif // STATEMENT_H