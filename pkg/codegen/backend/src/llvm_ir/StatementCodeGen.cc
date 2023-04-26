#include <sstream>

#include "llvm_ir/IRCodeGenVisitor.h"

/// @brief
/// @param stmt
/// @return
llvm::Value *IRCodegenVisitor::codegen(const AssignStmt &stmt) {
  auto variable = stmt.variable->codegen(*this);
  if (!variable) {
    std::ostringstream buf;
    buf << "attempt to assign to nonexistent LHS, ";
    buf << stmt.variable->name;

    throw IRCodegenException(buf.str());
  }

  auto val = stmt.value->codegen(*this);
  if (!val) {
    std::ostringstream buf;
    buf << "attempt to assign an invalid value, ";
    buf << stmt.value;

    throw IRCodegenException(buf.str());
  }

  builder->CreateStore(val, variable);

  return val;
}

/// @brief
/// @param stmt
/// @return
llvm::Value *IRCodegenVisitor::codegen(const ProcedureStatement &stmt) {
  if (stmt.name->name == "writeln") {
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
      auto load = builder->CreateLoad(val->getType(), val);

      argsV.push_back(load);
    }

    return builder->CreateCall(printfFunc, argsV, "call");
  }

  return nullptr;
}

llvm::Value *IRCodegenVisitor::codegen(const IfStatement& is) {
  auto condV = is.cond->codegen(*this);
  if (!condV)
    throw IRCodegenException("Null condition expr for if-else statement");;

  auto *TheFunction = builder->GetInsertBlock()->getParent();

  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*ctx, "then", TheFunction);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*ctx, "ifcont");
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