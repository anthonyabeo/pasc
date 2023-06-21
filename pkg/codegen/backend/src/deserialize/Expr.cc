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
  case Pasc::Operator_OpKind_Mult:
    return Operator::Mult;
  case Pasc::Operator_OpKind_FwdSlash:
    return Operator::FwdSlash;
  case Pasc::Operator_OpKind_Not:
    return Operator::Not;
  default:
    throw DeserializeProtobufException("invalid operator kind");
  }
}

/// @brief
/// @param expr
/// @return std::unique_ptr<Expr> a pointer to an expression type
std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression &expr) {
  switch (expr.kind()) {
  case Pasc::Expression_ExprKind_Var:
    return std::make_unique<IdentifierExpr>(expr);
  case Pasc::Expression_ExprKind_IdxVar:
    return std::make_unique<IndexedVarExpr>(expr.iv());
  case Pasc::Expression_ExprKind_Field:
    return std::make_unique<FieldDesigExpr>(expr.fld());
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
  case Pasc::Expression_ExprKind_Str:
    return std::make_unique<CharString>(expr.cs());
  case Pasc::Expression_ExprKind_UnExpr:
    return std::make_unique<UnaryExpression>(expr.ue());
  case Pasc::Expression_ExprKind_Bool:
    return std::make_unique<BoolExpr>(expr.bl());
  case Pasc::Expression_ExprKind_Rg:
    return std::make_unique<Range>(expr.rng());
  case Pasc::Expression_ExprKind_Nil:
    return std::make_unique<Nil>(expr.nl());
  default:
    throw DeserializeProtobufException("invalid expression kind");
  }
}

std::unique_ptr<Expr> deserializeVar(const Pasc::Expression &id) {
  switch (id.kind()) {
  case Pasc::Expression_ExprKind_IdxVar:
    return std::make_unique<IndexedVariable>(id.iv());
  case Pasc::Expression_ExprKind_Var:
    return std::make_unique<VariableID>(id.variable().name());
  case Pasc::Expression_ExprKind_Field:
    return std::make_unique<FieldDesignator>(id.fld());
  default:
    throw DeserializeProtobufException("invalid expression kind");
  }
};

//////////////////////////////
// UNSIGNED INTEGER LITERAL
//////////////////////////////
UIntegerLiteral::UIntegerLiteral(const Pasc::UIntLiteral &lit) {
  value = lit.value();
}

llvm::Value *UIntegerLiteral::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string UIntegerLiteral::get_name() {return "";}


///////////////////////////
// VARIABLE IDENTIFIER
///////////////////////////
VariableID::VariableID(const std::string &n) {
  name = n;
}

llvm::Value *VariableID::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string VariableID::get_name() {
  return name;
}

///////////////////////////
// INDEXED VARIABLE
///////////////////////////
IndexedVariable::IndexedVariable(const Pasc::IndexedVariable &iv) {
  arrayName = iv.arrayvar();
  for (int i = 0; i < iv.idxexpr_size(); ++i) {
    indices.push_back(deserializeExpr(iv.idxexpr(i)));
  }
}

llvm::Value *IndexedVariable::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string IndexedVariable::get_name() {
  return arrayName;
}

///////////////////////////
// FIELD DESIGNATOR
///////////////////////////
FieldDesignator::FieldDesignator(const Pasc::FieldDesignator &fd) {
  recordName = fd.recordvar();
  fieldSpec = deserializeExpr(fd.fieldspec());
}

llvm::Value *FieldDesignator::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string FieldDesignator::get_name() {
  return recordName;
}

///////////////////////////
// RVALUE IDENTIFIER
///////////////////////////
IdentifierExpr::IdentifierExpr(const Pasc::Expression &id) {
  identifier = deserializeVar(id);
}

llvm::Value *IdentifierExpr::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string IdentifierExpr::get_name() {
  return identifier->get_name();
}

///////////////////////////
// RVALUE INDEXED VARIABLE
///////////////////////////
IndexedVarExpr::IndexedVarExpr(const Pasc::IndexedVariable &iv) {
  arrayName = iv.arrayvar();
  for (int i = 0; i < iv.idxexpr_size(); ++i) {
    indices.push_back(deserializeExpr(iv.idxexpr(i)));
  }
}

llvm::Value *IndexedVarExpr::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string IndexedVarExpr::get_name() {
    return arrayName;
}

///////////////////////////
// RVALUE FIELD DESIGNATOR
///////////////////////////
FieldDesigExpr::FieldDesigExpr(const Pasc::FieldDesignator &fde) {
    recordName = fde.recordvar();
    fieldSpec = deserializeExpr(fde.fieldspec());
}

llvm::Value *FieldDesigExpr::codegen(IRVisitor &v) {
    return v.codegen(*this);
}

std::string FieldDesigExpr::get_name() {
    return recordName;
}

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

std::string BinaryExpression::get_name() {
  return "";
}

///////////////////////////
// FUNCTION CALL
///////////////////////////
FunctionCall::FunctionCall(const Pasc::FuncCall& fc) {
  name = deserializeVar(fc.name());
  for (int i = 0; i < fc.args_size(); ++i) {
    args.push_back(deserializeExpr(fc.args(i)));
  }
}

llvm::Value *FunctionCall::codegen(IRVisitor& v) {
  return v.codegen(*this);
}

std::string FunctionCall::get_name() {
  return "";
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

std::string WriteParameter::get_name() { return ""; }

///////////////////////////
// UREAL LITERAL
///////////////////////////
URealLiteral::URealLiteral(const Pasc::URealLiteral &ur) {
  value = ur.value();
}

llvm::Value *URealLiteral::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string URealLiteral::get_name() { return ""; }

///////////////////////////
// CHARACTER STRING
///////////////////////////
CharString::CharString(const Pasc::CharString &cs) {
  str = cs.value();
}

llvm::Value *CharString::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string CharString::get_name() { return str; }

///////////////////////////
// UNARY EXPRESSION
///////////////////////////
UnaryExpression::UnaryExpression(const Pasc::UnaryExpr &ue) {
  op = deserializeOp(ue.op());
  operand = deserializeExpr(ue.operand());
}

llvm::Value *UnaryExpression::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string UnaryExpression::get_name() { return ""; }

///////////////////////////
// BOOLEAN EXPRESSION
///////////////////////////
BoolExpr::BoolExpr(const Pasc::BoolExpr &bl) {
  value = bl.value();
}

llvm::Value *BoolExpr::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string BoolExpr::get_name() { return ""; }

///////////////////////////
// RANGE
///////////////////////////
Range::Range(const Pasc::Range &rng) {
  start = deserializeExpr(rng.start());
  end = deserializeExpr(rng.end());
}

llvm::Value *Range::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string Range::get_name() { return ""; }

///////////////////////////
// NIL
///////////////////////////
Nil::Nil(const Pasc::NilValue &nil) {
  name = nil.name();
}

llvm::Value *Nil::codegen(IRVisitor &v) { return v.codegen(*this); }

std::string Nil::get_name() { return name; }