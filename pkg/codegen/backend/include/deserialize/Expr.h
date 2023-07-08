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
  virtual std::string get_name() = 0;
  virtual llvm::Value *codegen(IRVisitor &v) = 0;
};

std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression &);

std::unique_ptr<Expr> deserializeVar(const Pasc::Expression &id);

struct VariableID : public Expr {
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
  std::string get_name() override;
};


struct BinaryExpression : public Expr {
  enum Operator op;
  std::unique_ptr<Expr> left;
  std::unique_ptr<Expr> right;

  explicit BinaryExpression(const Pasc::BinaryExpr&);
  llvm::Value* codegen(IRVisitor&) override;
  std::string get_name() override;
};

struct IdentifierExpr : public Expr {
  std::unique_ptr<Expr> identifier;

  explicit IdentifierExpr(const Pasc::Expression&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
};

struct IndexedVarExpr : public Expr {
  std::string arrayName;
  std::vector<std::unique_ptr<Expr>> indices;

  explicit IndexedVarExpr(const Pasc::IndexedVariable&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
};

struct FieldDesigExpr : public Expr {
  std::string recordName;
  uint64_t fieldSpec;

  explicit FieldDesigExpr(const Pasc::FieldDesignator&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
};

struct Nil : public Expr {
  std::string name;

  explicit Nil(const Pasc::NilValue&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
};

///////////////////////////
// FUNCTION CALL
///////////////////////////
struct FunctionCall : public Expr {
  std::unique_ptr<Expr> name;
  std::vector<std::unique_ptr<Expr>> args;
  std::unique_ptr<Type> ret_type;

  explicit FunctionCall(const Pasc::FuncCall&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
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
  std::string get_name() override;
};

///////////////////////////
// UREAL LITERAL
///////////////////////////
struct URealLiteral : public Expr {
  double value;

  explicit URealLiteral(const Pasc::URealLiteral &);
  llvm::Value *codegen(IRVisitor &) override;
  std::string get_name() override;
};

///////////////////////////
// CHARACTER STRING
///////////////////////////
struct CharString : public Expr {
  std::string str;

  explicit CharString(const Pasc::CharString&);
  llvm::Value *codegen(IRVisitor &) override;
  std::string get_name() override;
};

///////////////////////////
// UNARY EXPRESSION
///////////////////////////
struct UnaryExpression : public Expr {
  enum Operator op;
  std::unique_ptr<Expr> operand;

  explicit UnaryExpression(const Pasc::UnaryExpr&);
  llvm::Value* codegen(IRVisitor&) override;
  std::string get_name() override;
};

///////////////////////////
// BOOLEAN EXPRESSION
///////////////////////////
struct BoolExpr : public Expr {
  bool value;

  explicit BoolExpr(const Pasc::BoolExpr&);
  llvm::Value *codegen(IRVisitor &) override;
  std::string get_name() override;
};

///////////////////////////
// INDEXED VARIABLE
///////////////////////////
struct IndexedVariable : public Expr {
  std::string arrayName;
  std::vector<std::unique_ptr<Expr>> indices;

  explicit IndexedVariable(const Pasc::IndexedVariable&);
  llvm::Value *codegen(IRVisitor&) override;
  std::string get_name() override;
};

///////////////////////////
// FIELD DESIGNATOR
///////////////////////////
struct FieldDesignator : public Expr {
  std::string recordName;
  uint64_t fieldSpec;

  explicit FieldDesignator(const Pasc::FieldDesignator&);
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
  std::string get_name() override;
};

#endif // EXPR_H