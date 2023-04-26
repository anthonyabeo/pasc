#include "llvm_ir/IRCodeGenVisitor.h"

llvm::Value *IRCodegenVisitor::codegen(const IdentifierIR &id) {
  auto alloca = symTable->Resolve(id.name);
  if (!alloca) {
    throw IRCodegenException("undefined name, " + id.name);
  }

  return alloca;
}

llvm::Value *IRCodegenVisitor::codegen(const UIntegerLiteral &uilit) {
  return llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*ctx),
                                      uilit.value);
}

llvm::Value *IRCodegenVisitor::codegen(const BinaryExpression &binExpr) {
  auto *L = binExpr.left->codegen(*this);
  auto *R = binExpr.right->codegen(*this);
  
  if(!L || !R)
    return nullptr;

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