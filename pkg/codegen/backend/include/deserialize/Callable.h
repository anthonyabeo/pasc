#ifndef PASC_CALLABLE_H
#define PASC_CALLABLE_H

#include <memory>

#include "llvm/IR/Value.h"

#include "Type.h"
#include "Block.h"
#include "Deserializer.h"

class IRVisitor;

struct FormalParameter {
  virtual ~FormalParameter() = default;
  virtual std::string getName() = 0;
  virtual std::vector<llvm::Type*> codegen(IRVisitor &v) = 0;
};

struct ValueParam : public FormalParameter {
  std::vector<std::string> names;
  std::unique_ptr<Type> type;

  std::string getName() override;
  explicit ValueParam(const Pasc::ValueParam&);
  std::vector<llvm::Type*> codegen(IRVisitor &) override;
};

struct VariableParam : public FormalParameter {
  std::vector<std::string> names;
  std::unique_ptr<Type> type;

  std::string getName() override;
  explicit VariableParam(const Pasc::VariableParam&);
  std::vector<llvm::Type*> codegen(IRVisitor &) override;
};

struct FuncHeading : public FormalParameter {
  std::string name;
  std::vector<std::unique_ptr<FormalParameter>> params;
  std::unique_ptr<Type> retType;

  std::string getName() override;
  explicit FuncHeading(const Pasc::FuncHeading&);
  std::vector<llvm::Type*> codegen(IRVisitor &) override;
};

struct ProcHeading : public FormalParameter {
  std::string name;
  std::vector<std::unique_ptr<FormalParameter>> params;
  std::unique_ptr<Type> retType;

  std::string getName() override;
  explicit ProcHeading(const Pasc::ProcHeading&);
  std::vector<llvm::Type*> codegen(IRVisitor &) override;
};

struct FunctionDeclaration : public Callable {
  std::unique_ptr<Block> blk;
  std::unique_ptr<FuncHeading> funcHead;
  std::string directive;

  void call() override;
  std::string getName() override;
  explicit FunctionDeclaration(const Pasc::FuncDeclaration&);
  llvm::Value *codegen(IRVisitor &) override;
};

struct ProcedureDeclaration : public Callable {
  std::unique_ptr<ProcHeading> procHead;
  std::unique_ptr<Block> blk;
  std::string directive;

  void call() override;
  std::string getName() override;
  explicit ProcedureDeclaration(const Pasc::ProcDeclaration&);
  llvm::Value *codegen(IRVisitor &) override;
};
#endif // PASC_CALLABLE_H
