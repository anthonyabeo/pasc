#ifndef PASC_CALLABLE_H
#define PASC_CALLABLE_H

#include <memory>

#include "Type.h"
#include "Block.h"
#include "Deserializer.h"

//struct Callable {
//  virtual ~Callable() = default;
//  virtual void call() = 0;
//};
//
//std::unique_ptr<Callable> deserializeCallable(const Pasc::Callable&);

struct FormalParameter {
  virtual ~FormalParameter() = default;
  virtual void formalParam() = 0;
};

struct ValueParam : public FormalParameter {
  std::vector<std::string> names;
  std::unique_ptr<Type> type;

  void formalParam() override;
  explicit ValueParam(const Pasc::ValueParam&);
};

struct VariableParam : public FormalParameter {
  std::vector<std::string> names;
  std::unique_ptr<Type> type;

  void formalParam() override;
  explicit VariableParam(const Pasc::VariableParam&);
};

struct FuncHeading : public FormalParameter {
  std::string name;
  std::vector<std::unique_ptr<FormalParameter>> params;
  std::unique_ptr<Type> retType;

  void formalParam() override;
  explicit FuncHeading(const Pasc::FuncHeading&);
};

struct ProcHeading : public FormalParameter {
  std::string name;
  std::vector<std::unique_ptr<FormalParameter>> params;

  void formalParam() override;
  explicit ProcHeading(const Pasc::ProcHeading&);
};

struct FunctionDeclaration : public Callable {
  std::unique_ptr<Block> blk;
  std::unique_ptr<FuncHeading> funcHead;
  std::string directive;

  void call() override;
  explicit FunctionDeclaration(const Pasc::FuncDeclaration&);
};

struct ProcedureDeclaration : public Callable {
  std::unique_ptr<ProcHeading> procHead;
  std::unique_ptr<Block> blk;
  std::string directive;

  void call() override;
  explicit ProcedureDeclaration(const Pasc::ProcDeclaration&);
};
#endif // PASC_CALLABLE_H
