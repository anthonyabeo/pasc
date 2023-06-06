#ifndef EXPR_H
#define EXPR_H

#include <string>

#include "../proto/program.pb.h"
#include "llvm/IR/Value.h"

#include "Type.h"

class IRVisitor;

enum class Operator {
  Plus,
  Minus,
  Div,
  Mod,
  And,
  Or,
  In,
  Equal,
  Less,
  Great,
  GreatEqual,
  LessEqual,
  LessGreat,
  Mult,
  FwdSlash,
  Not
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
  std::unique_ptr<Type> ret_type;

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

///////////////////////////
// UREAL LITERAL
///////////////////////////
struct URealLiteral : public Expr {
  double value;

  explicit URealLiteral(const Pasc::URealLiteral &);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// CHARACTER STRING
///////////////////////////
struct CharString : public Expr {
  std::string str;

  explicit CharString(const Pasc::CharString&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// UNARY EXPRESSION
///////////////////////////
struct UnaryExpression : public Expr {
  enum Operator op;
  std::unique_ptr<Expr> operand;

  explicit UnaryExpression(const Pasc::UnaryExpr&);
  llvm::Value* codegen(IRVisitor&) override;
};

///////////////////////////
// BOOLEAN EXPRESSION
///////////////////////////
struct BoolExpr : public Expr {
  bool value;

  explicit BoolExpr(const Pasc::BoolExpr&);
  llvm::Value *codegen(IRVisitor &) override;
};

///////////////////////////
// INDEXED VARIABLE
///////////////////////////
struct IndexedVariable : public Identifier {
  std::string arrayName;
  std::vector<std::unique_ptr<Expr>> indices;

  explicit IndexedVariable(const Pasc::Identifier_IndexedVariable&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
};

///////////////////////////
// FIELD DESIGNATOR
///////////////////////////
struct FieldDesignator : public Identifier {
  std::string recordName;
  std::unique_ptr<Expr> fieldSpec;

  explicit FieldDesignator(const Pasc::Identifier_FieldDesignator&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
};

///////////////////////////
// RANGE
//////////////////////////
struct Range : public Expr {
  std::unique_ptr<Expr> start;
  std::unique_ptr<Expr> end;

  explicit Range(const Pasc::Range&);
  llvm::Value* codegen(IRVisitor&) override;
};

#endif // EXPR_H