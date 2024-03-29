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
  virtual std::shared_ptr<LLVMScope> GetEnclosingScope() = 0;
  virtual void Define(std::string, llvm::AllocaInst *) = 0;
  virtual llvm::AllocaInst* Resolve(std::string) = 0;
};

class LLVMSymbolTable : public LLVMScope {
private:
  std::string name;
  std::map<std::string, llvm::AllocaInst*> symbols;
  std::shared_ptr<LLVMScope> parent;

public:
  LLVMSymbolTable()= default;
  explicit LLVMSymbolTable(std::string);
  ~LLVMSymbolTable() override= default;

  void setParent(const std::shared_ptr<LLVMScope>&);

  std::string GetScopeName() override;
  std::shared_ptr<LLVMScope> GetEnclosingScope() override;
  void Define(std::string, llvm::AllocaInst *) override;
  llvm::AllocaInst* Resolve(std::string) override;
};

#endif // SYMBOL_TABLE_H