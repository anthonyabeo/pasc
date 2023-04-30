#include <memory>

#include "program.pb.h"

#include "deserialize/Deserializer.h"
#include "deserialize/Expr.h"
#include "deserialize/IRVisitor.h"

enum Operator deserializeOp(const Pasc::Operator& opt) {
  switch (opt.op()) {
  case Pasc::Operator_OpKind_Less:
    return Operator::Less;
  case Pasc::Operator_OpKind_Great:
    return Operator::Great;
  default:
    throw DeserializeProtobufException("invalid operator kind");
  }
}

/// @brief
/// @param expr
/// @return std::unique_ptr<Expr> a pointer to an expression type
std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression &expr) {
  switch (expr.kind()) {
  case Pasc::Expression_ExprKind_Ident:
    return std::make_unique<IdentifierExpr>(expr.id());
  case Pasc::Expression_ExprKind_UInt:
    return std::make_unique<UIntegerLiteral>(expr.uint());
  case Pasc::Expression_ExprKind_BinExpr:
    return std::make_unique<BinaryExpression>(expr.be());
  default:
    throw DeserializeProtobufException("invalid expression kind");
  }
}

std::unique_ptr<Identifier> deserializeID(const Pasc::Identifier &id) {
  switch (id.kind()) {
  case Pasc::Identifier_IDKind_EntireVar:
    return std::make_unique<VariableID>(id.var().name());
  case Pasc::Identifier_IDKind_IdxVar:
    break ;
  case Pasc::Identifier_IDKind_Field:
    break ;
  default:
    throw DeserializeProtobufException("invalid identifier kind");
  }

  return nullptr;
};

//////////////////////////////
// UNSIGNED INTEGER LITERAL
//////////////////////////////
UIntegerLiteral::UIntegerLiteral(const Pasc::UIntLiteral &lit) {
  value = lit.value();
}

llvm::Value *UIntegerLiteral::codegen(IRVisitor &v) { return v.codegen(*this); }


///////////////////////////
// VARIABLE IDENTIFIER
///////////////////////////
VariableID::VariableID(const std::string &n) {name = n; }
llvm::Value *VariableID::codegen(IRVisitor &v) { return v.codegen(*this); }
std::string VariableID::get_name() { return name; }

///////////////////////////
// RVALUE IDENTIFIER
///////////////////////////
IdentifierExpr::IdentifierExpr(const Pasc::Identifier &id) {identifier = deserializeID(id); }

llvm::Value *IdentifierExpr::codegen(IRVisitor &v) { return v.codegen(*this); }

///////////////////////////
// BINARY EXPRESSION
///////////////////////////
BinaryExpression::BinaryExpression(const Pasc::BinaryExpr& expr) {
  op = deserializeOp(expr.op());
  left = deserializeExpr(expr.left());
  right = deserializeExpr(expr.right());
}

llvm::Value *BinaryExpression::codegen(IRVisitor &v) {
  return v.codegen(*this);
}