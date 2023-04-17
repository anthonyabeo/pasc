#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "deserialize/Program.h"
#include "llvm_ir/IRCodeGenVisitor.h"
#include "llvm_ir/SymbolTable.h"

#include <memory>

IRCodegenVisitor::IRCodegenVisitor(std::string& moduleName) {
  ctx = std::make_unique<llvm::LLVMContext>();
  builder = std::make_unique<llvm::IRBuilder<>>(*ctx);
  module = std::make_unique<llvm::Module>(moduleName, *ctx);

  symTable = std::make_unique<LLVMSymbolTable>("main", nullptr);
}

void IRCodegenVisitor::codegenProgram(const ProgramIR &program) {
  llvm::FunctionType *mainType = llvm::FunctionType::get(
      llvm::IntegerType::getInt32Ty(*ctx),
      std::vector<llvm::Type *>(),
      false);

  llvm::Function *main = llvm::Function::Create(
      mainType,
      llvm::Function::ExternalLinkage,
      "main",
      module.get());

  llvm::BasicBlock *entryBB =
      llvm::BasicBlock::Create(*ctx, "entry", main);
  builder->SetInsertPoint(entryBB);

  codegenBlock(*program.block);

  llvm::APInt retVal(32,(uint32_t)0, true);
  builder->CreateRet(llvm::ConstantInt::get(*(ctx), retVal));
}

void IRCodegenVisitor::dumpLLVMIR() { module->print(llvm::outs(), nullptr); }


std::string IRCodegenVisitor::dumpLLVMIRToString() {
  std::string out_str;
  llvm::raw_string_ostream oss(out_str);

  module->print(oss, nullptr);

  return oss.str();
}

llvm::Type* IRCodegenVisitor::getLLVMTypeOf(const Type &t) {
   if (t.GetName() == "integer") {
     return llvm::Type::getInt32Ty(*ctx);
   } else {
     return nullptr;
   }
 }

void IRCodegenVisitor::codegenBlock(const Block &blk) {
  for (auto &varDecl : blk.VarDeclrs) {
     auto typ = getLLVMTypeOf(*varDecl->type);

     llvm::Function *TheFunction = builder->GetInsertBlock()->getParent();
     llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                           TheFunction->getEntryBlock().begin());

    auto alloca = TmpB.CreateAlloca(typ,
                                    nullptr, varDecl->name->name);

    symTable->Define(varDecl->name->name, alloca);
  }

  for (auto &stmt : blk.Stmts) {
    stmt->codegen(*this);
  }
}