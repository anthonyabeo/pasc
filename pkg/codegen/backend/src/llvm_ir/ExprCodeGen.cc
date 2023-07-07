#include "llvm_ir/IRCodeGenVisitor.h"

llvm::Value *IRCodegenVisitor::codegen(const VariableID &id) {
  auto alloca = curScope->Resolve(id.name);
  if (!alloca) {
    throw IRCodegenException("undefined name, " + id.name);
  }

  return alloca;
}

llvm::Value* IRCodegenVisitor::codegen(const IdentifierExpr &expr) {
  auto id = curScope->Resolve(expr.identifier->get_name());
  if (!id) {
    throw IRCodegenException(
        std::string("Identifier not found: " + expr.identifier->get_name()));
  }

  llvm::Value *idVal = builder->CreateLoad(id->getAllocatedType(), id, "ld");
  if (!idVal) {
    throw IRCodegenException(
        std::string("cannot read value of: " + expr.identifier->get_name()));
  }

  return idVal;
}

llvm::Value *IRCodegenVisitor::codegen(const UIntegerLiteral &lit) {
  return llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx),
                                lit.value,
                                false);
}

llvm::Value *IRCodegenVisitor::codegen(const BinaryExpression &binExpr) {
  auto *L = binExpr.left->codegen(*this);
  auto *R = binExpr.right->codegen(*this);
  
  if(!L || !R)
    throw IRCodegenException("at least one operand of null");

  switch (binExpr.op) {
  case Operator::Less:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateCmp(llvm::CmpInst::Predicate::FCMP_ULT,
                                FL, FR);
    }

    return builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_SLT,
                              L, R, "cond");
  case Operator::Great:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateCmp(llvm::CmpInst::Predicate::FCMP_UGT,
                                FL, FR);
    }

    return builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_SGT,
                              L, R, "cond");
  case Operator::GreatEqual:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateCmp(llvm::CmpInst::Predicate::FCMP_UGE,
                                FL, FR);
    }

    return builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_SGE,
                              L, R, "cond");
  case Operator::LessEqual:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateCmp(llvm::CmpInst::Predicate::FCMP_ULE,
                                FL, FR);
    }

    return builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_SLE,
                              L, R, "cond");
  case Operator::Plus:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateFAdd(FL, FR, "fadd");
    }

    return builder->CreateAdd(L, R, "add");
  case Operator::Minus:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateFSub(FL, FR, "fsub");
    }

    return builder->CreateSub(L, R, "sub");
  case Operator::Equal:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateCmp(llvm::CmpInst::Predicate::FCMP_UEQ,
                                FL, FR);
    }

    return builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, L, R);
  case Operator::Mod:
    return builder->CreateSRem(L, R);
  case Operator::Mult:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateFMul(FL, FR, "fmult");
    }

    return builder->CreateMul(L, R, "mult");
  case Operator::And:
    // TODO implement logical AND
  case Operator::Or:
    // TODO implement logical OR
  case Operator::Div:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateFDiv(FL, FR, "fdiv");
    }

    return builder->CreateSDiv(L, R, "div");
  case Operator::FwdSlash: {
    auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
    auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

    return builder->CreateFDiv(FL, FR);
  }
  case Operator::In:
  case Operator::LessGreat:
    if (L->getType() == llvm::Type::getDoubleTy(*ctx) || R->getType() == llvm::Type::getDoubleTy(*ctx)) {
      auto FL = builder->CreateSIToFP(L, llvm::Type::getDoubleTy(*ctx));
      auto FR = builder->CreateSIToFP(R, llvm::Type::getDoubleTy(*ctx));

      return builder->CreateCmp(llvm::CmpInst::Predicate::FCMP_UNE,
                                FL, FR);
    }
    
    return builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_NE, L, R);
  default:
    throw IRCodegenException("invalid binary operator");
  }
}

llvm::Value *IRCodegenVisitor::codegen(const FunctionCall& fc) {
  std::vector<llvm::Value*> funcArgs;
  funcArgs.reserve(fc.args.size());

  for (auto& arg :fc.args) {
    funcArgs.push_back(arg->codegen(*this));
  }

  auto f = module->getFunction(fc.name->get_name());

  return builder->CreateCall(f, funcArgs);
}

llvm::Value *IRCodegenVisitor::codegen(const WriteParameter& wp) {
  return wp.e->codegen(*this);
}

llvm::Value *IRCodegenVisitor::codegen(const URealLiteral &ur) {
  return llvm::ConstantFP::get(llvm::Type::getDoubleTy(*ctx), ur.value);
}

llvm::Value *IRCodegenVisitor::codegen(const CharString &cs) {
  auto charType = llvm::IntegerType::getInt8Ty(*ctx);

  //1. Initialize chars vector
  std::vector<llvm::Constant *> chars(cs.str.size());
  for(unsigned int i = 0; i < cs.str.size(); i++)
    chars[i] = llvm::ConstantInt::get(charType, cs.str[i]);

  //1b. add a zero terminator too
  chars.push_back(llvm::ConstantInt::get(charType, 0));

  //2. Initialize the string from the characters
  auto stringType = llvm::ArrayType::get(charType, chars.size());

  //3. Create the declaration statement
  auto gv = (llvm::GlobalVariable*) module->getOrInsertGlobal("", stringType);
  gv->setInitializer(llvm::ConstantArray::get(stringType, chars));
  gv->setConstant(true);
  gv->setLinkage(llvm::GlobalValue::LinkageTypes::InternalLinkage);
  gv->setUnnamedAddr (llvm::GlobalValue::UnnamedAddr::Global);

  return llvm::ConstantExpr::getBitCast(gv, charType->getPointerTo());
}

llvm::Value *IRCodegenVisitor::codegen(const UnaryExpression &ue) {
  auto operand = ue.operand->codegen(*this);
  if(!operand)
    throw IRCodegenException("operand is null");

  switch (ue.op) {
  case Operator::Minus:
    return builder->CreateNeg(operand);
  case Operator::Plus:
    return operand;
  case Operator::Not:
    return builder->CreateNot(operand, "not");
  default:
    throw IRCodegenException("invalid unary operator");
  }
}

llvm::Value *IRCodegenVisitor::codegen(const BoolExpr &be) {
  uint v = 0;
  if(be.value){
    v = 1;
  }

  return llvm::ConstantInt::get(llvm::Type::getInt1Ty(*ctx), v,false);
}

llvm::Value *IRCodegenVisitor::codegen(const Range &rng) {
  std::vector<llvm::Constant*> range;

  auto start = rng.start->codegen(*this);
  auto st = dyn_cast<llvm::ConstantInt>(start);

  auto end = rng.end->codegen(*this);
  auto ed = dyn_cast<llvm::ConstantInt>(end);
  for (uint64_t i = st->getValue().getSExtValue(); i < ed->getValue().getSExtValue(); ++i) {
    range.push_back(llvm::ConstantInt::get(llvm::Type::getInt32Ty(*ctx), i));
  }

  return llvm::ConstantVector::get(range);
}

llvm::Value *IRCodegenVisitor::codegen(const IndexedVariable &iv) {
  auto array = curScope->Resolve(iv.arrayName);
  if (!array) {
    throw IRCodegenException("undefined array, " + iv.arrayName);
  }

  auto index = iv.indices[0]->codegen(*this);
  if (!index) {
    throw IRCodegenException("invalid array index");
  }

  auto GEP = builder->CreateGEP(index->getType(), array, index);

  for (int i = 1; i < iv.indices.size(); ++i) {
    index = iv.indices[i]->codegen(*this);
    if (!index)
      throw IRCodegenException("invalid array index");

    GEP = builder->CreateGEP(index->getType(), GEP, index);
  }

  return GEP;
}

llvm::Value *IRCodegenVisitor::codegen(const IndexedVarExpr &ive) {
  auto array = curScope->Resolve(ive.arrayName);
  if (!array) {
    throw IRCodegenException("undefined array, " + ive.arrayName);
  }

  auto index = ive.indices[0]->codegen(*this);
  if (!index) {
    throw IRCodegenException("invalid array index");
  }

  auto GEP = builder->CreateGEP(index->getType(), array, index);
  for (int i = 1; i < ive.indices.size(); ++i) {
    index = ive.indices[i]->codegen(*this);
    if (!index)
      throw IRCodegenException("invalid array index");

    GEP = builder->CreateGEP(index->getType(), GEP, index);
  }

  llvm::Value *idVal = builder->CreateLoad(index->getType(), GEP);
  if (!idVal) {
    throw IRCodegenException(std::string("cannot read value of: " + ive.arrayName));
  }

  return idVal;
}

llvm::Value *IRCodegenVisitor::codegen(const FieldDesignator &fd) {
  auto record = curScope->Resolve(fd.recordName);
  if(!record)
    throw IRCodegenException("undefined record, " + fd.recordName);

  auto field = fd.fieldSpec->codegen(*this);
  auto f = llvm::dyn_cast<llvm::ConstantInt>(field);

  return builder->CreateStructGEP(record->getAllocatedType(), record, f->getSExtValue());
}

llvm::Value *IRCodegenVisitor::codegen(const FieldDesigExpr &fde) {
  auto record = curScope->Resolve(fde.recordName);
  if(!record)
    throw IRCodegenException("undefined record, " + fde.recordName);

  auto field = fde.fieldSpec->codegen(*this);
  auto f = llvm::dyn_cast<llvm::ConstantInt>(field);

  auto GEP = builder->CreateStructGEP(record->getAllocatedType(), record, f->getSExtValue());

  llvm::Value *idVal = builder->CreateLoad(f->getType(), GEP);
  if (!idVal)
    throw IRCodegenException(std::string("cannot read value of: " + fde.recordName));

  return idVal;
}

llvm::Value *IRCodegenVisitor::codegen(const Nil &nil) {
  return llvm::ConstantPointerNull::get(llvm::PointerType::get(*ctx, 0));
}