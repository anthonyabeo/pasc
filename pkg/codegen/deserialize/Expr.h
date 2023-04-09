#ifndef EXPR_H
#define EXPR_H

#include <string>

// #include "IRVisitor.h"
#include "program.pb.h"
#include "llvm/IR/Value.h"

class IRVisitor;

struct Expr {
  virtual ~Expr() = default;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
};

std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression &);

/// @brief Identifier denotes a user-defined, non-keyword symbol
struct Identifier : public Expr {
  std::string name;

  Identifier(const Pasc::Identifier &);
  virtual llvm::Value *codegen(IRVisitor &) override;
};

/// @brief UIntegerLiteral denoted an unsigned 32-bit integer literal value.
struct UIntegerLiteral : public Expr {
  int value;

  UIntegerLiteral(const Pasc::UIntLiteral &);
  virtual llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// STATEMENTS
///////////////////////////
struct Statement {
  virtual ~Statement() = default;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
};

std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement &);

struct AssignStmt : public Statement {
private:
  std::unique_ptr<Identifier> variable;
  std::unique_ptr<Expr> value;

public:
  AssignStmt(const Pasc::AssignStmt &stmt);
  virtual llvm::Value *codegen(IRVisitor &) override;
};

struct ProcedureStatement : public Statement {
  std::unique_ptr<Identifier> name;
  std::vector<std::unique_ptr<Expr>> params;

  ProcedureStatement(const Pasc::ProcedureStmt &stmt);
  virtual llvm::Value *codegen(IRVisitor &) override;
};
#endif // EXPR_H