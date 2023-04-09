#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"

#include "IRCodeGenVisitor.h"

IRCodegenVisitor::IRCodegenVisitor() {
  ctx = std::unique_ptr<llvm::LLVMContext>(new llvm::LLVMContext());
  builder = std::unique_ptr<llvm::IRBuilder<>>(new llvm::IRBuilder<>(*ctx));
}

void IRCodegenVisitor::codegenProgram(const ProgramIR &program) {
  module = std::unique_ptr<llvm::Module>(new llvm::Module(program.name, *ctx));

  llvm::FunctionType *mainType =
      llvm::FunctionType::get(llvm::IntegerType::getInt32Ty(*ctx),
                              std::vector<llvm::Type *>(), false /* isVarArgs */
      );

  llvm::Function *main = llvm::Function::Create(
      mainType, llvm::Function::ExternalLinkage, "main", module.get());

  llvm::BasicBlock *mainBasicBlock =
      llvm::BasicBlock::Create(*ctx, "entry", main);
  builder->SetInsertPoint(mainBasicBlock);

  for (auto &stmt : program.block->Stmts) {
    stmt->codegen(*this);
  }

  llvm::APInt retVal(32 /* bitSize */, (uint32_t)0, true /* signed */);
  builder->CreateRet(llvm::ConstantInt::get(*(ctx), retVal));
}

void IRCodegenVisitor::dumpLLVMIR() { module->print(llvm::outs(), nullptr); }

/////////////////////
// STATEMENTS
////////////////////

/// @brief
/// @param stmt
/// @return
llvm::Value *IRCodegenVisitor::codegen(const AssignStmt &stmt) {
  return nullptr;
}

/// @brief
/// @param stmt
/// @return
llvm::Value *IRCodegenVisitor::codegen(const ProcedureStatement &stmt) {
  return nullptr;
}

////////////////////////
// EXPRESSIONS
////////////////////////
llvm::Value *IRCodegenVisitor::codegen(const IdentifierIR &id) {
  return nullptr;
}

llvm::Value *IRCodegenVisitor::codegen(const UIntegerLiteral &uintlit) {
  return llvm::ConstantInt::getSigned(llvm::Type::getInt32Ty(*ctx),
                                      uintlit.value);
}