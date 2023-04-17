#include "IRCodeGenVisitor.h"

llvm::Value *IRCodegenVisitor::codegen(const IdentifierIR &id) {
  auto alloca = symTable->Resolve(id.name);
  if (alloca == nullptr) {
    throw IRCodegenException("undefined name, " + id.name);
  }

  return alloca.get();
}

llvm::Value *IRCodegenVisitor::codegen(const UIntegerLiteral &uilit) {
  return llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*ctx),
                                      uilit.value);
}