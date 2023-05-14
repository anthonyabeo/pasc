#include <sstream>

#include "llvm/IR/Verifier.h"

#include "llvm_ir/IRCodeGenVisitor.h"

/// @brief
/// @param stmt
/// @return
llvm::Value *IRCodegenVisitor::codegen(const AssignStmt &stmt) {
  auto variable = stmt.variable->codegen(*this);
  if (!variable) {
    throw IRCodegenException(
      "attempt to assign to nonexistent LHS, " + stmt.variable->get_name());
  }

  auto val = stmt.value->codegen(*this);
  if (!val) {
    std::ostringstream buf;
    buf << "attempt to assign an invalid value, ";
    buf << stmt.value;

    throw IRCodegenException(buf.str());
  }

  return builder->CreateStore(val, variable);

//  return val;
}

/// @brief
/// @param stmt
/// @return
llvm::Value *IRCodegenVisitor::codegen(const Writeln &stmt) {
    std::vector<llvm::Type *> printfArgsTypes = {llvm::Type::getInt8PtrTy(*ctx)};
    auto printfFunc = module->getOrInsertFunction(
        "printf",
        llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx),
                                  printfArgsTypes,
                                  true));

    // The format string for the printf function, declared as a global literal
    llvm::Value *str = builder->CreateGlobalStringPtr("%d\n", "str");


    std::vector<llvm::Value *> argsV({str});
    for (auto& p : stmt.params) {
      auto val = p->codegen(*this);
      argsV.push_back(val);
    }

    return builder->CreateCall(printfFunc, argsV, "call");
}

llvm::Value *IRCodegenVisitor::codegen(const IfStatement& is) {
  auto condV = is.cond->codegen(*this);
  if (!condV)
    throw IRCodegenException("Null condition expr for if-else statement");;

  auto *TheFunction = builder->GetInsertBlock()->getParent();

  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*ctx, "then", TheFunction);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*ctx, "cont");
  llvm::BasicBlock *ElseBB = nullptr;
  if(is.else_path) {
    ElseBB = llvm::BasicBlock::Create(*ctx, "else");
  }

  builder->CreateCondBr(condV, ThenBB, ElseBB);

  // Emit then value.
  builder->SetInsertPoint(ThenBB);

  auto *ThenV = is.true_path->codegen(*this);
  if (!ThenV)
    return nullptr;

  builder->CreateBr(MergeBB);

  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = builder->GetInsertBlock();

  // Emit else block.
  llvm::Value* ElseV = nullptr;
  if(is.else_path) {
    TheFunction->insert(TheFunction->end(), ElseBB);
    builder->SetInsertPoint(ElseBB);

    ElseV = is.else_path->codegen(*this);
    if (!ElseV)
      return nullptr;

    builder->CreateBr(MergeBB);
    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = builder->GetInsertBlock();
  }

  // Emit merge block.
  TheFunction->insert(TheFunction->end(), MergeBB);
  builder->SetInsertPoint(MergeBB);

  if (ThenV->getType() == llvm::Type::getVoidTy(*ctx) ||
      ElseV->getType() == llvm::Type::getVoidTy(*ctx) ||
      (ThenV->getType() != ElseV->getType())) {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*ctx));
  }

  auto *PN = builder->CreatePHI(ThenV->getType(), 2, "iftmp");
  PN->addIncoming(ThenV, ThenBB);
  if(is.else_path)
    PN->addIncoming(ElseV, ElseBB);

  return PN;
}

llvm::Value *IRCodegenVisitor::codegen(const FunctionDeclaration &fd) {
  auto parentInsertBB = builder->GetInsertBlock();

  auto s = std::make_shared<LLVMSymbolTable>(fd.funcHead->name);
  s->setParent(curScope);
  curScope = s;

  ////////////////////
  // FUNCTION TYPE
  ///////////////////
  std::vector<llvm::Type*> paramsTypes;
  for (auto& param :fd.funcHead->params) {
    auto p = param->codegen(*this);

    paramsTypes.reserve(paramsTypes.size() + distance(std::begin(p) , std::end(p)));
    paramsTypes.insert(paramsTypes.end(), std::begin(p), std::end(p));
  }

  auto *funcType = llvm::FunctionType::get(
      fd.funcHead->retType->codegen(*this),
      paramsTypes,
      false);

  ///////////////////////
  // FUNCTION PROTOTYPE
  ///////////////////////
  auto *F = llvm::Function::Create(
      funcType,
      llvm::Function::ExternalLinkage,
      fd.funcHead->name,
      module.get());

  ///////////////////////
  // FUNCTION DEFINITION
  ///////////////////////
  llvm::BasicBlock *entryBB =
      llvm::BasicBlock::Create(*ctx, "entry", F);
  builder->SetInsertPoint(entryBB);

  for (std::size_t i = 0; i < F->arg_size(); ++i) {
    auto FPName = fd.funcHead->params[i]->getName();

    if (FPName == "VALUE_PARAM") {
      auto valParam = dynamic_cast<ValueParam*>(fd.funcHead->params[i].get());
      for (int j = 0; j < valParam->names.size(); ++j) {
        auto Arg = F->getArg(j);
        auto name = valParam->names[j];

        llvm::Function *TheFunction = builder->GetInsertBlock()->getParent();
        auto alloca = CreateEntryBlockAlloca(
                          TheFunction, name, F->getFunctionType()->getParamType(j));

        curScope->Define(name, alloca);
        builder->CreateStore(Arg, curScope->Resolve(name));
      }
      i += valParam->names.size();
    }
    else if (FPName == "VAR_PARAM") {

    }
    else if (FPName == "FUNC_HEAD") {

    }
    else {

    }

  }
  codegenBlock(*fd.blk);

  llvm::verifyFunction(F->getFunction());

  curScope = curScope->GetEnclosingScope();
  // restore it here
  builder->SetInsertPoint(parentInsertBB);
  return F;
}

llvm::Value *IRCodegenVisitor::codegen(const ProcedureDeclaration &) {
  return nullptr;
}

llvm::Value *IRCodegenVisitor::codegen(const ProcedureStmt& ps) {
  return nullptr;
}

llvm::Value *IRCodegenVisitor::codegen(const ReturnStatement& ps) {
  auto ret = ps.value->codegen(*this);
  return builder->CreateRet(ret);
}