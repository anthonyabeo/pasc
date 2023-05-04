#ifndef EXPR_H
#define EXPR_H

#include <string>

#include "../program.pb.h"
#include "llvm/IR/Value.h"

class IRVisitor;

enum class Operator {
  Plus,
  Minus,
  Div,
  Sub,
  Mod,
  And,
  Or,
  In,
  Equal,
  Less,
  Great
};

enum Operator deserializeOp(const Pasc::Operator&);

struct Expr {
  virtual ~Expr() = default;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
};

std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression &);

/// @brief Identifier denotes a user-defined, non-keyword symbol
struct Identifier {
  virtual ~Identifier()  = default;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
  virtual std::string get_name() = 0;
};

std::unique_ptr<Identifier> deserializeID(const Pasc::Identifier &id);

struct VariableID : public Identifier {
  std::string name;

  explicit VariableID(const std::string&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
};


/// @brief UIntegerLiteral denoted an unsigned 32-bit integer literal value.
struct UIntegerLiteral : public Expr {
  uint value;

  explicit UIntegerLiteral(const Pasc::UIntLiteral &);
  llvm::Value *codegen(IRVisitor &) override;
};


struct BinaryExpression : public Expr {
  enum Operator op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;

  explicit BinaryExpression(const Pasc::BinaryExpr&);
  llvm::Value* codegen(IRVisitor&) override;
};

struct IdentifierExpr : public Expr {
  std::unique_ptr<Identifier> identifier;

  explicit IdentifierExpr(const Pasc::Identifier&);
  llvm::Value *codegen(IRVisitor&) override;
};

///////////////////////////
// FUNCTION CALL
///////////////////////////
struct FunctionCall : public Expr {
  std::unique_ptr<Identifier> name;
  std::vector<std::unique_ptr<Expr>> args;

  explicit FunctionCall(const Pasc::FuncCall&);
  llvm::Value *codegen(IRVisitor&) override;
};

///////////////////////////
// WRITE PARAMETER
///////////////////////////
struct WriteParameter : public Expr {
  std::unique_ptr<Expr> e;
  std::unique_ptr<Expr> totalWidth;
  std::unique_ptr<Expr> fracDigits;

  explicit WriteParameter(const Pasc::WriteParameter&);
  llvm::Value *codegen(IRVisitor&) override;
};

#endif // EXPR_H