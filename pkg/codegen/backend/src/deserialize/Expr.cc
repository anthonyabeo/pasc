#include <memory>

#include "proto/program.pb.h"
#include "proto/expression.pb.h"

#include "deserialize/Deserializer.h"
#include "deserialize/Expr.h"
#include "deserialize/IRVisitor.h"

enum Operator deserializeOp(const Pasc::Operator& opt) {
  switch (opt.op()) {
  case Pasc::Operator_OpKind_Less:
    return Operator::Less;
  case Pasc::Operator_OpKind_Great:
    return Operator::Great;
  case Pasc::Operator_OpKind_GreatEqual:
    return Operator::GreatEqual;
  case Pasc::Operator_OpKind_LessEqual:
    return Operator::LessEqual;
  case Pasc::Operator_OpKind_LessGreat:
    return Operator::LessGreat;
  case Pasc::Operator_OpKind_Plus:
    return Operator::Plus;
  case Pasc::Operator_OpKind_Minus:
    return Operator::Minus;
  case Pasc::Operator_OpKind_Div:
    return Operator::Div;
  case Pasc::Operator_OpKind_Mod:
    return Operator::Mod;
  case Pasc::Operator_OpKind_And:
    return Operator::And;
  case Pasc::Operator_OpKind_Or:
    return Operator::Or;
  case Pasc::Operator_OpKind_In:
    return Operator::In;
  case Pasc::Operator_OpKind_Equal:
    return Operator::Equal;
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
  case Pasc::Expression_ExprKind_UReal:
    return std::make_unique<URealLiteral>(expr.ureal());
  case Pasc::Expression_ExprKind_BinExpr:
    return std::make_unique<BinaryExpression>(expr.be());
  case Pasc::Expression_ExprKind_WriteParam:
    return std::make_unique<WriteParameter>(expr.wp());
  case Pasc::Expression_ExprKind_FCall:
    return std::make_unique<FunctionCall>(expr.fc());
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

///////////////////////////
// FUNCTION CALL
///////////////////////////
FunctionCall::FunctionCall(const Pasc::FuncCall& fc) {
  name = deserializeID(fc.name().id());
  for (int i = 0; i < fc.args_size(); ++i) {
    args.push_back(deserializeExpr(fc.args(i)));
  }
}

llvm::Value *FunctionCall::codegen(IRVisitor& v) {
  return v.codegen(*this);
}

///////////////////////////
// WRITE PARAMETER
///////////////////////////
WriteParameter::WriteParameter(const Pasc::WriteParameter& wp) {
  e = deserializeExpr(wp.e());
  if(wp.has_totalwidth())
    totalWidth = deserializeExpr(wp.totalwidth());

  if(wp.has_fracdigits())
    fracDigits = deserializeExpr(wp.fracdigits());
}

llvm::Value *WriteParameter::codegen(IRVisitor& v) {
  return v.codegen(*this);
}

///////////////////////////
// UREAL LITERAL
///////////////////////////
URealLiteral::URealLiteral(const Pasc::URealLiteral &ur) {
  value = ur.value();
}

llvm::Value *URealLiteral::codegen(IRVisitor &v) {
  return v.codegen(*this);
}