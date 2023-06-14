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

  llvm::Value *idVal = builder->CreateLoad(id->getAllocatedType(), id);
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
  return builder->CreateCmp(
        llvm::CmpInst::Predicate::ICMP_SLT, L, R, "cond");
  case Operator::Great:
  return builder->CreateCmp(
      llvm::CmpInst::Predicate::ICMP_SGT, L, R, "cond");
  case Operator::GreatEqual:
  return builder->CreateCmp(
      llvm::CmpInst::Predicate::ICMP_SGE, L, R, "cond");
  case Operator::LessEqual:
    return builder->CreateCmp(
      llvm::CmpInst::Predicate::ICMP_SLE, L, R, "cond");
  case Operator::Plus:
    return builder->CreateAdd(L, R, "add");
  case Operator::Minus:
    return builder->CreateSub(L, R, "sub");
  case Operator::Equal:
    return builder->CreateCmp(llvm::CmpInst::Predicate::ICMP_EQ, L, R);
  case Operator::Mod:
    return builder->CreateSRem(L, R);
  case Operator::Mult:
    return builder->CreateMul(L, R, "mult");
  case Operator::And:
    return builder->CreateAnd(L, R, "and");
  case Operator::Or:
    return builder->CreateOr(L, R, "or");
  case Operator::Div:
    return builder->CreateSDiv(L, R, "div");
  case Operator::FwdSlash:
    return builder->CreateFDiv(L, R);
  case Operator::In:
  case Operator::LessGreat:
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
  auto gv = (llvm::GlobalVariable*) module->getOrInsertGlobal(".str", stringType);
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

  // TODO generalize to n-D arrays
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

  // TODO generalize to n-D arrays
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
  return nullptr;
}

llvm::Value *IRCodegenVisitor::codegen(const FieldDesigExpr &fde) {
  return nullptr;
}
