#include <sstream>

#include "llvm_ir/IRCodeGenVisitor.h"

/// @brief
/// @param stmt
/// @return
llvm::Value *IRCodegenVisitor::codegen(const AssignStmt &stmt) {
  auto alloca = stmt.variable->codegen(*this);
  if (!alloca) {
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

  return builder->CreateStore(val, alloca);
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