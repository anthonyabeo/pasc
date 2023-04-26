#include "llvm_ir/SymbolTable.h"

#include <utility>

LLVMSymbolTable::LLVMSymbolTable(std::string n) {
  name = std::move(n);
  parent = nullptr;
}

std::string LLVMSymbolTable::GetScopeName() { return name; }

std::shared_ptr<LLVMScope> LLVMSymbolTable::GetEnclosingScope() {
  return parent;
}

void LLVMSymbolTable::Define(std::string n, llvm::AllocaInst *inst) {
  symbols.insert({n, inst});
}

llvm::AllocaInst* LLVMSymbolTable::Resolve(std::string n) {
  auto it = symbols.find(n);
  if (it != symbols.end()) {
    return it->second;
  }

  if (!parent) {
    return parent->Resolve(name);
  }

  return nullptr;
}