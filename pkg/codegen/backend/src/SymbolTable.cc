#include "SymbolTable.h"

LLVMSymbolTable::LLVMSymbolTable(std::string n, LLVMSymbolTable *p) {
  name = n;
  parent = std::unique_ptr<LLVMSymbolTable>(p);
}

std::string LLVMSymbolTable::GetScopeName() { return name; }

std::unique_ptr<LLVMScope> LLVMSymbolTable::GetEnclosingScope() {
  return parent;
}

void LLVMSymbolTable::Define(std::string name, llvm::AllocaInst *inst) {
  symbols.insert({name, std::unique_ptr<llvm::AllocaInst>(inst)});
}

std::unique_ptr<llvm::AllocaInst> LLVMSymbolTable::Resolve(std::string name) {
  auto sym = symbols[name];
  if (sym != nullptr) {
    return sym;
  }

  if (parent != nullptr) {
    return parent->Resolve(name);
  }

  return nullptr;
}