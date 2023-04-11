#ifndef SYMBOL_TABLE_H
#define SYMBOL_TABLE_H

#include <map>
#include <string>

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

class LLVMScope {
public:
  virtual ~LLVMScope() = default;

  virtual std::string GetScopeName() = 0;
  virtual std::unique_ptr<LLVMScope> GetEnclosingScope() = 0;
  virtual void Define(std::string, llvm::AllocaInst *) = 0;
  virtual std::unique_ptr<llvm::AllocaInst> Resolve(std::string) = 0;
};

class LLVMSymbolTable : public LLVMScope {
private:
  std::string name;
  std::map<std::string, std::unique_ptr<llvm::AllocaInst>> symbols;
  std::unique_ptr<LLVMScope> parent;

public:
  LLVMSymbolTable(){};
  LLVMSymbolTable(std::string, LLVMSymbolTable *);
  virtual ~LLVMSymbolTable(){};

  virtual std::string GetScopeName() override;
  virtual std::unique_ptr<LLVMScope> GetEnclosingScope() override;
  virtual void Define(std::string, llvm::AllocaInst *) override;
  virtual std::unique_ptr<llvm::AllocaInst> Resolve(std::string) override;
};

#endif // SYMBOL_TABLE_H