#include "llvm_ir/IRCodeGenVisitor.h"

llvm::Type *IRCodegenVisitor::codegen(const IntegerType &typeIR) {
  return llvm::Type::getInt32Ty(*ctx);
}

llvm::Type *IRCodegenVisitor::codegen(const BoolType &typeIR) {
  return llvm::Type::getInt1Ty(*ctx);
};

llvm::Type *IRCodegenVisitor::codegen(const VoidType &typeIR) {
    return llvm::Type::getVoidTy(*ctx);
};

////////////////////
// PARAMETER TYPES
////////////////////
std::vector<llvm::Type*> IRCodegenVisitor::codegen(const ValueParam &v) {
  std::vector<llvm::Type*> paramTypes;

  auto typ = v.type->codegen(*this);
  for (int i = 0; i < v.names.size(); ++i) {
    paramTypes.push_back(typ);
  }

  return paramTypes;
}

std::vector<llvm::Type*> IRCodegenVisitor::codegen(const VariableParam &typeIR) {
  std::vector<llvm::Type*> paramTypes;

  return paramTypes;
}

std::vector<llvm::Type*> IRCodegenVisitor::codegen(const FuncHeading &typeIR) {
  std::vector<llvm::Type*> paramTypes;

  return paramTypes;
}

std::vector<llvm::Type*> IRCodegenVisitor::codegen(const ProcHeading &typeIR) {
  std::vector<llvm::Type*> paramTypes;

  return paramTypes;
}