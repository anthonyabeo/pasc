#include <sstream>

#include "llvm/IR/Verifier.h"

#include "llvm_ir/IRCodeGenVisitor.h"


llvm::Value *IRCodegenVisitor::codegen(const AssignStmt &stmt) {
  if(!stmt.label.empty()) {
    llvm::BasicBlock* Label;
    Label = GetBBFromLabel(stmt.label);

    // set it as the current insertion point
    builder->SetInsertPoint(Label);
  }

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

  builder->CreateStore(val, variable);

  return val;
}

llvm::Value *IRCodegenVisitor::codegen(const Writeln &stmt) {
  if(!stmt.label.empty()) {
    llvm::BasicBlock* Label;
    Label = GetBBFromLabel(stmt.label);

    // set it as the current insertion point
    builder->SetInsertPoint(Label);
  }

  std::vector<llvm::Type *> printfArgsTypes = {llvm::Type::getInt8PtrTy(*ctx)};
  auto printfFunc = module->getOrInsertFunction(
      "printf",
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx),
                                printfArgsTypes,
                                true));

  std::vector<llvm::Value *> argsV;

  // The format string for the printf function, declared as a global literal
  llvm::Value *str = stmt.params[0]->codegen(*this);
  argsV.push_back(str);

  for (int i = 1; i < stmt.params.size(); ++i) {
    auto val = stmt.params[i]->codegen(*this);
    argsV.push_back(val);
  }

  return builder->CreateCall(printfFunc, argsV, "call");
}

llvm::Value *IRCodegenVisitor::codegen(const Write &stmt) {
  if(!stmt.label.empty()) {
    llvm::BasicBlock* Label;
    Label = GetBBFromLabel(stmt.label);

    // set it as the current insertion point
    builder->SetInsertPoint(Label);
  }

  std::vector<llvm::Type *> printfArgsTypes = {llvm::Type::getInt8PtrTy(*ctx)};
  auto printfFunc = module->getOrInsertFunction(
      "printf",
      llvm::FunctionType::get(llvm::Type::getInt32Ty(*ctx),
                              printfArgsTypes,
                              true));

  std::vector<llvm::Value *> argsV;

  // The format string for the printf function, declared as a global literal
  llvm::Value *str = stmt.params[0]->codegen(*this);
  argsV.push_back(str);

  for (int i = 1; i < stmt.params.size(); ++i) {
    auto val = stmt.params[i]->codegen(*this);
    argsV.push_back(val);
  }

  return builder->CreateCall(printfFunc, argsV, "call");
}

llvm::Value *IRCodegenVisitor::codegen(const IfStatement& is)  {
  if(!is.label.empty()) {
    llvm::BasicBlock* Label;
    Label = GetBBFromLabel(is.label);

    // set it as the current insertion point
    builder->SetInsertPoint(Label);
  }

  auto condV = is.cond->codegen(*this);
  if (!condV)
    throw IRCodegenException("Null condition expr for if-else statement");

  auto *TheFunction = builder->GetInsertBlock()->getParent();

  llvm::BasicBlock *ThenBB = llvm::BasicBlock::Create(*ctx, "then", TheFunction);
  llvm::BasicBlock *MergeBB = llvm::BasicBlock::Create(*ctx, "cont");
  llvm::BasicBlock *ElseBB = nullptr;
  if(is.else_path) {
    ElseBB = llvm::BasicBlock::Create(*ctx, "else");
    builder->CreateCondBr(condV, ThenBB, ElseBB);
  } else {
    builder->CreateCondBr(condV, ThenBB, MergeBB);
  }

  // Emit then value.
  builder->SetInsertPoint(ThenBB);

  auto *ThenV = is.true_path->codegen(*this);
  if (!ThenV)
    throw IRCodegenException("then-path codegen error");

  // Codegen of 'Then' can change the current block, update ThenBB for the PHI.
  ThenBB = builder->GetInsertBlock();
  builder->CreateBr(MergeBB);

  // Emit else block.
  llvm::Value* ElseV = nullptr;
  if(is.else_path) {
    TheFunction->insert(TheFunction->end(), ElseBB);
    builder->SetInsertPoint(ElseBB);

    ElseV = is.else_path->codegen(*this);
    if (!ElseV)
      throw IRCodegenException("else-path codegen error");

    // Codegen of 'Else' can change the current block, update ElseBB for the PHI.
    ElseBB = builder->GetInsertBlock();
    builder->CreateBr(MergeBB);
  }

  // Emit merge block.
  TheFunction->insert(TheFunction->end(), MergeBB);
  builder->SetInsertPoint(MergeBB);

  if (ThenV->getType() == llvm::Type::getVoidTy(*ctx) || !ElseV ||
      ElseV->getType() == llvm::Type::getVoidTy(*ctx) ||
      (ThenV->getType() != ElseV->getType())) {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*ctx));
  }

  llvm::PHINode *PN = builder->CreatePHI(ThenV->getType(), 2, "iftmp");
  PN->addIncoming(ThenV, ThenBB);
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
  
  builder->SetInsertPoint(parentInsertBB);
  return F;
}

llvm::Value *IRCodegenVisitor::codegen(const ProcedureDeclaration &pd) {
  auto parentInsertBB = builder->GetInsertBlock();

  auto s = std::make_shared<LLVMSymbolTable>(pd.procHead->name);
  s->setParent(curScope);
  curScope = s;

  ////////////////////
  // FUNCTION TYPE
  ///////////////////
  std::vector<llvm::Type*> paramsTypes;
  for (auto& param :pd.procHead->params) {
    auto p = param->codegen(*this);

    paramsTypes.reserve(paramsTypes.size() + distance(std::begin(p) , std::end(p)));
    paramsTypes.insert(paramsTypes.end(), std::begin(p), std::end(p));
  }

  auto *procType = llvm::FunctionType::get(
      llvm::Type::getVoidTy(*ctx),
      paramsTypes,
      false);

  ///////////////////////
  // PROCEDURE PROTOTYPE
  ///////////////////////
  auto *F = llvm::Function::Create(
      procType,
      llvm::Function::ExternalLinkage,
      pd.procHead->name,
      module.get());

  ///////////////////////
  // FUNCTION DEFINITION
  ///////////////////////
  llvm::BasicBlock *entryBB =
      llvm::BasicBlock::Create(*ctx, "entry", F);
  builder->SetInsertPoint(entryBB);

  for (std::size_t i = 0; i < F->arg_size(); ++i) {
    auto FPName = pd.procHead->params[i]->getName();

    if (FPName == "VALUE_PARAM") {
      auto valParam = dynamic_cast<ValueParam*>(pd.procHead->params[i].get());
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
  codegenBlock(*pd.blk);

  llvm::verifyFunction(F->getFunction());

  curScope = curScope->GetEnclosingScope();

  builder->SetInsertPoint(parentInsertBB);
  return F;
}

llvm::Value *IRCodegenVisitor::codegen(const ProcedureStmt& ps) {
  return nullptr;
}

llvm::Value *IRCodegenVisitor::codegen(const ReturnStatement& rs) {
  if(!rs.label.empty()) {
    llvm::BasicBlock* Label;
    Label = GetBBFromLabel(rs.label);

    // set it as the current insertion point
    builder->SetInsertPoint(Label);
  }

  auto ret = rs.value->codegen(*this);
  builder->CreateRet(ret);

  return ret;
}

llvm::Value *IRCodegenVisitor::codegen(const WhileStatement &ws) {
  if(!ws.label.empty()) {
    llvm::BasicBlock* Label;
    Label = GetBBFromLabel(ws.label);

    // set it as the current insertion point
    builder->SetInsertPoint(Label);
  }

  auto CondV = ws.cond->codegen(*this);
  if(!CondV)
    throw IRCodegenException("Null condition expr for while statement");

  auto TheFunction = builder->GetInsertBlock()->getParent();
  auto BodyBB = llvm::BasicBlock::Create(*ctx, "while_body", TheFunction);
  auto ContBB = llvm::BasicBlock::Create(*ctx, "cont", TheFunction);

  builder->CreateCondBr(CondV, BodyBB, ContBB);

  builder->SetInsertPoint(BodyBB);
  auto BodyV = ws.body->codegen(*this);
  if (!BodyV)
    throw IRCodegenException("Null body for while statement");

  CondV = ws.cond->codegen(*this);
  if(!CondV)
    throw IRCodegenException("Null condition expr for while statement");

  BodyBB = builder->GetInsertBlock();
  builder->CreateCondBr(CondV, BodyBB, ContBB);

  builder->SetInsertPoint(ContBB);

  return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*ctx));
}

llvm::Value *IRCodegenVisitor::codegen(const CompoundStatement &cs) {
    if(!cs.label.empty()) {
      llvm::BasicBlock* Label;
      Label = GetBBFromLabel(cs.label);

      // set it as the current insertion point
      builder->SetInsertPoint(Label);
    }

    for (auto& stmt : cs.stmts) {
      stmt->codegen(*this);
    }

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*ctx));
}

llvm::Value *IRCodegenVisitor::codegen(const RepeatStatement &rs) {
    if(!rs.label.empty()) {
      llvm::BasicBlock* Label;
      Label = GetBBFromLabel(rs.label);

      // set it as the current insertion point
      builder->SetInsertPoint(Label);
    }

    auto TheFunction = builder->GetInsertBlock()->getParent();
    auto BodyBB = llvm::BasicBlock::Create(*ctx, "body", TheFunction);
    auto ContBB = llvm::BasicBlock::Create(*ctx, "cont", TheFunction);

    // jump into and start executing the body
    builder->CreateBr(BodyBB);

    // generate code for the body under the 'body' label
    builder->SetInsertPoint(BodyBB);
    for (auto& stmt : rs.stmts) {
      stmt->codegen(*this);
    }

    // generate conditional branch to regulate loop
    auto CondV = rs.cond->codegen(*this);
    if(!CondV)
      throw IRCodegenException("Null condition expr for repeat statement");

    BodyBB = builder->GetInsertBlock();
    builder->CreateCondBr(CondV, ContBB, BodyBB);

    // point the builder which block to go to next
    builder->SetInsertPoint(ContBB);

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*ctx));
}

llvm::Value *IRCodegenVisitor::codegen(const ForStatement &fs) {
    if(!fs.label.empty()) {
      llvm::BasicBlock* Label;
      Label = GetBBFromLabel(fs.label);

      // set it as the current insertion point
      builder->SetInsertPoint(Label);
    }

    auto TheFunction = builder->GetInsertBlock()->getParent();
    auto BodyBB = llvm::BasicBlock::Create(*ctx, "body", TheFunction);
    auto ContBB = llvm::BasicBlock::Create(*ctx, "cont", TheFunction);

    // initialize the ctlVar with the initValue
    auto CtlVar = curScope->Resolve(fs.ctlVar->get_name());
    if (!CtlVar)
      throw IRCodegenException("Null loop control variable in for-statement");

    auto InitVal = fs.initValue->codegen(*this);
    if(!InitVal)
      throw IRCodegenException("Null initial value in for-statement");

    builder->CreateStore(InitVal, CtlVar);

    // read the value of CtlValue for use in the rest of the loop
    llvm::Value *CtlVal = builder->CreateLoad(CtlVar->getAllocatedType(), CtlVar);

    // create condition where ctlVar is compared to finalValue
    // if dir is TO, then the ctlVar will be increment. If it is DOWN_TO, it will be decremented.
    auto FinalVal = fs.finalValue->codegen(*this);
    if(!FinalVal)
      throw IRCodegenException("Null final value in for-statement");

    llvm::Value* CondV;
    if (fs.dir == Pasc::TokenKind::TO) {
      CondV = builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_SLE, CtlVal,
                                 FinalVal);
    } else if(fs.dir == Pasc::TokenKind::DOWN_TO) {
      CondV = builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_SGE, CtlVal,
                                 FinalVal);
    } else{
      throw IRCodegenException("Invalid dir value in for-statement");
    }
    if(!CondV)
      throw IRCodegenException("Null condition expr for repeat statement");

    builder->CreateCondBr(CondV, BodyBB, ContBB);

    // codegen for body
    builder->SetInsertPoint(BodyBB);
    auto BodyV = fs.body->codegen(*this);
    if (!BodyV)
      throw IRCodegenException("Null body for while statement");

    BodyBB = builder->GetInsertBlock();

    // update control variable
    CtlVal = builder->CreateLoad(CtlVar->getAllocatedType(), CtlVar);

    llvm::APInt One(32,1, true);
    if (fs.dir == Pasc::TokenKind::TO) {
      auto Val = builder->CreateAdd(CtlVal, llvm::Constant::getIntegerValue(llvm::Type::getInt32Ty(*ctx), One));
      builder->CreateStore(Val, CtlVar);

      CondV = builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_SLE, Val,
                                 FinalVal);
    } else {
      auto Val = builder->CreateSub(CtlVal, llvm::Constant::getIntegerValue(llvm::Type::getInt32Ty(*ctx), One));
      builder->CreateStore(Val, CtlVar);
      CondV = builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_SGE, Val,
                                 FinalVal);
    }

    builder->CreateCondBr(CondV, BodyBB, ContBB);

    // point the builder which block to go to next
    builder->SetInsertPoint(ContBB);

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*ctx));
}

llvm::Value *IRCodegenVisitor::codegen(const GotoStatement &gs) {
    llvm::BasicBlock* Label = GetBBFromLabel(gs.label);
    return builder->CreateBr(Label);
}

llvm::Value *IRCodegenVisitor::codegen(const CaseStatement &cs) {
    if(!cs.label.empty()) {
      llvm::BasicBlock* Label;
      Label = GetBBFromLabel(cs.label);

      // set it as the current insertion point
      builder->SetInsertPoint(Label);
    }

    auto CurrentInsertBB = builder->GetInsertBlock();

    auto TheFunction = CurrentInsertBB->getParent();
    auto ContBlk = llvm::BasicBlock::Create(*ctx, "cont", TheFunction);
    auto DefaultBlk = llvm::BasicBlock::Create(*ctx, "default", TheFunction);
    builder->SetInsertPoint(DefaultBlk);
    builder->CreateBr(ContBlk);

    builder->SetInsertPoint(CurrentInsertBB);
    llvm::Value* value = cs.index->codegen(*this);
    if(!value)
      throw IRCodegenException("Invalid case-index in case statement");

    auto switchStmt = builder->CreateSwitch(
        value, DefaultBlk, cs.caseListElems.size());

    for (auto& cle : cs.caseListElems) {
      auto Block = llvm::BasicBlock::Create(*ctx, "", TheFunction);
      builder->SetInsertPoint(Block);

      auto BodyV = cle.body->codegen(*this);
      if(!BodyV)
        throw IRCodegenException("Invalid case-body in case statement");
      builder->CreateBr(ContBlk);

      for (auto&c :cle.consts) {
        auto TagV = c->codegen(*this);

        auto Tag = dyn_cast<llvm::ConstantInt>(TagV);
        if (!Tag) {
          throw IRCodegenException("invalid tag");
        }

        switchStmt->addCase(Tag, Block);
      }
    }

    builder->SetInsertPoint(ContBlk);

    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*ctx));
}

llvm::Value *IRCodegenVisitor::codegen(const WithStatement &ws) {
    return llvm::Constant::getNullValue(llvm::Type::getInt32Ty(*ctx));
}