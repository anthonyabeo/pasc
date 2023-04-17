#ifndef EXPR_H
#define EXPR_H

#include <string>

#include "program.pb.h"
#include "llvm/IR/Value.h"

class IRVisitor;

struct Expr {
  virtual ~Expr() = default;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
};

std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression &);

/// @brief Identifier denotes a user-defined, non-keyword symbol
struct IdentifierIR : public Expr {
  std::string name;

  explicit IdentifierIR(const Pasc::Expression &);
  llvm::Value *codegen(IRVisitor &) override;
};

/// @brief UIntegerLiteral denoted an unsigned 32-bit integer literal value.
struct UIntegerLiteral : public Expr {
  int value;

  explicit UIntegerLiteral(const Pasc::UIntLiteral &);
  llvm::Value *codegen(IRVisitor &) override;
};

#endif // EXPR_H