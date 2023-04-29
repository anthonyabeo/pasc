#include "llvm_ir/IRCodeGenVisitor.h"

llvm::Value *IRCodegenVisitor::codegen(const IdentifierIR &id) {
  auto alloca = symTable->Resolve(id.name);
  if (!alloca) {
    throw IRCodegenException("undefined name, " + id.name);
  }

  return alloca;
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
  case Pasc::TokenKind::LESS:
  return builder->CreateCmp(
        llvm::CmpInst::Predicate::ICMP_SLT, L, R, "cond");
  case Pasc::TokenKind::GREAT:
  return builder->CreateCmp(
      llvm::CmpInst::Predicate::ICMP_SGT, L, R, "cond");
  default:
    throw IRCodegenException("invalid binary operator");
  }
}