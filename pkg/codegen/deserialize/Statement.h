#ifndef STATEMENT_H
#define STATEMENT_H

// #include "llvm/IR/Value.h"

// #include "Expr.h"
// #include "IRVisitor.h"

// class IRVisitor;

// struct Statement {
//   virtual ~Statement() = default;
//   virtual llvm::Value *codegen(IRVisitor &v) = 0;
// };

// std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &);

// struct AssignStmt : public Statement {
// private:
//   std::unique_ptr<Identifier> variable;
//   std::unique_ptr<Expr> value;

// public:
//   AssignStmt(const Pasc::AssignStmt &stmt);
//   virtual llvm::Value *codegen(IRVisitor &) override;
// };

// struct ProcedureStatement : public Statement {
//   std::unique_ptr<Identifier> name;
//   std::vector<std::unique_ptr<Expr>> params;

//   ProcedureStatement(const Pasc::ProcedureStmt &stmt);
//   virtual llvm::Value *codegen(IRVisitor &) override;
// };

#endif // STATEMENT_H