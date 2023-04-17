#include "llvm_ir/SymbolTable.h"

LLVMSymbolTable::LLVMSymbolTable(std::string n, LLVMSymbolTable *p) {
  name = n;
  parent = std::shared_ptr<LLVMSymbolTable>(p);
}

std::string LLVMSymbolTable::GetScopeName() { return name; }

std::shared_ptr<LLVMScope> LLVMSymbolTable::GetEnclosingScope() {
  return parent;
}

void LLVMSymbolTable::Define(std::string n, llvm::AllocaInst *inst) {
  symbols.insert({n, std::unique_ptr<llvm::AllocaInst>(inst)});
}

std::shared_ptr<llvm::AllocaInst> LLVMSymbolTable::Resolve(std::string n) {
  auto it = symbols.find(n);
  if (it != symbols.end()) {
    return it->second;
  }

  if (parent != nullptr) {
    return parent->Resolve(name);
  }

  return nullptr;
}