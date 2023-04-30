#include "llvm_ir/IRCodeGenVisitor.h"

llvm::Value *IRCodegenVisitor::codegen(const VariableID &id) {
  auto alloca = symTable->Resolve(id.name);
  if (!alloca) {
    throw IRCodegenException("undefined name, " + id.name);
  }

  return alloca;
}

llvm::Value* IRCodegenVisitor::codegen(const IdentifierExpr &expr) {
  auto id = symTable->Resolve(expr.identifier->get_name());
  if (!id) {
    throw IRCodegenException(
        std::string("Identifier not found: " + expr.identifier->get_name()));
  }

  llvm::Value *idVal = builder->CreateLoad(id->getAllocatedType(), id);
  if (!idVal) {
    throw IRCodegenException(
        std::string("cannot read value of: " + expr.identifier->get_name()));
  }

  return idVal;
}

llvm::Value *IRCodegenVisitor::codegen(const UIntegerLiteral &lit) {
  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx),
                                lit.value,
                                false);
}

llvm::Value *IRCodegenVisitor::codegen(const BinaryExpression &binExpr) {
  auto *L = binExpr.left->codegen(*this);
  auto *R = binExpr.right->codegen(*this);
  
  if(!L || !R)
    throw IRCodegenException("at least one operand of null");

  switch (binExpr.op) {
  case Operator::Less:
  return builder->CreateCmp(
        llvm::CmpInst::Predicate::ICMP_SLT, L, R, "cond");
  case Operator::Great:
  return builder->CreateCmp(
      llvm::CmpInst::Predicate::ICMP_SGT, L, R, "cond");
  default:
    throw IRCodegenException("invalid binary operator");
  }
}