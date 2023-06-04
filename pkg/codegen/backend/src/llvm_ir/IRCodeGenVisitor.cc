#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Verifier.h"

#include "deserialize/Program.h"
#include "llvm_ir/IRCodeGenVisitor.h"
#include "llvm_ir/SymbolTable.h"

#include <memory>

IRCodegenVisitor::IRCodegenVisitor(std::string& moduleName) {
  ctx = std::make_unique<llvm::LLVMContext>();
  module = std::make_unique<llvm::Module>(moduleName, *ctx);
  builder = std::make_unique<llvm::IRBuilder<>>(*ctx);

  symTable = std::make_shared<LLVMSymbolTable>("main");
  curScope = symTable;
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

  llvm::verifyFunction(main->getFunction());
}

void IRCodegenVisitor::dumpLLVMIR() { module->print(llvm::outs(), nullptr); }


std::string IRCodegenVisitor::dumpLLVMIRToString() {
  std::string out_str;
  llvm::raw_string_ostream oss(out_str);

  module->print(oss, nullptr);

  return oss.str();
}

llvm::AllocaInst *IRCodegenVisitor::CreateEntryBlockAlloca(
    llvm::Function *TheFunction, llvm::StringRef Name, llvm::Type* type)
{
  llvm::IRBuilder<> TmpB(&TheFunction->getEntryBlock(),
                   TheFunction->getEntryBlock().begin());
  return TmpB.CreateAlloca(type, nullptr, Name);
}

llvm::BasicBlock *IRCodegenVisitor::GetBBFromLabel(const std::string& label) {
  llvm::BasicBlock* basicBlock = nullptr;

  auto TheFunction = builder->GetInsertBlock()->getParent();
  for (auto& BB : *TheFunction) {
    if(BB.getName() == label) {
      basicBlock = &BB;
      break;
    }
  }

  if(!basicBlock)
    throw IRCodegenException("invalid label "+ label);

  return basicBlock;
}

void IRCodegenVisitor::codegenBlock(const Block &blk) {
  for (auto& label : blk.Labels) {
    auto TheFunction = builder->GetInsertBlock()->getParent();
    llvm::BasicBlock::Create(*ctx, label, TheFunction);
  }

  for (auto&constDef : blk.Consts) {
    auto ConstV = constDef->value->codegen(*this);
    if(!ConstV)
      throw IRCodegenException("invalid constant value");

    llvm::Function *TheFunction = builder->GetInsertBlock()->getParent();
    auto alloca = CreateEntryBlockAlloca(TheFunction, constDef->name, ConstV->getType());
    curScope->Define(constDef->name, alloca);

    builder->CreateStore(ConstV, alloca);
  }

  for (auto &varDecl : blk.VarDeclrs) {
    auto typ = varDecl->type->codegen(*this);

    llvm::Function *TheFunction = builder->GetInsertBlock()->getParent();
    auto alloca = CreateEntryBlockAlloca(TheFunction, varDecl->name->name, typ);

    curScope->Define(varDecl->name->name, alloca);
  }

  for (auto& call : blk.callables) {
    call->codegen(*this);
  }

  for (auto &stmt : blk.Stmts) {
    stmt->codegen(*this);
  }
}