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