// #include "llvm/IR/Constants.h"
// #include "llvm/IR/IRBuilder.h"
// #include "llvm/IR/LLVMContext.h"
// #include "llvm/IR/Module.h"
// #include "llvm/IR/Type.h"
// #include "llvm/IR/Value.h"

// #include "IRCodeGenVisitor.h"

// llvm::Value *IRCodegenVisitor::codegen(const Identifier &id) { return
// nullptr; }

// llvm::Value *IRCodegenVisitor::codegen(const UIntegerLiteral &uintlit) {
//   return llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*ctx),
//                                       uintlit.value);
// }