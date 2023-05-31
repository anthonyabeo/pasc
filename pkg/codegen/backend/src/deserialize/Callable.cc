#include "llvm/IR/Value.h"

#include "deserialize/IRVisitor.h"
#include "deserialize/Callable.h"

std::unique_ptr<Callable> deserializeCallable(const Pasc::Callable& call) {
  switch (call.kind()) {
  case Pasc::Callable_CallKind_Func:
    return std::make_unique<FunctionDeclaration>(call.funcdecl());
  case Pasc::Callable_CallKind_Proc:
    return std::make_unique<ProcedureDeclaration>(call.procdecl());
  default:
    throw DeserializeProtobufException("invalid callable kind");
  }
}

std::unique_ptr<FormalParameter> deserializeFormalParam(const Pasc::FormalParameter& fp) {
  switch (fp.kind()) {
  case Pasc::FormalParameter_FPKind_FuncHead:
    return std::make_unique<FuncHeading>(fp.fhead());
  case Pasc::FormalParameter_FPKind_ProcHead:
    return std::make_unique<ProcHeading>(fp.phead());
  case Pasc::FormalParameter_FPKind_ValueParam:
    return std::make_unique<ValueParam>(fp.valparam());
  case Pasc::FormalParameter_FPKind_VarParam:
    return std::make_unique<VariableParam>(fp.vparam());
  default:
    throw DeserializeProtobufException("invalid formal param kind");
  }
}

//////////////////////////
// FUNCTION DECLARATION
//////////////////////////
void FunctionDeclaration::call() {}

std::string FunctionDeclaration::getName() {
  return funcHead->getName();
}

FunctionDeclaration::FunctionDeclaration(const Pasc::FuncDeclaration& fd) {
  funcHead = std::make_unique<FuncHeading>(fd.funcheading());
  if(fd.has_blk())
    blk = std::make_unique<Block>(fd.blk());
  directive = fd.dir();
}

llvm::Value *FunctionDeclaration::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

//////////////////////////
// PROCEDURE DECLARATION
//////////////////////////
void ProcedureDeclaration::call() {}

std::string ProcedureDeclaration::getName() {
  return procHead->getName();
}

ProcedureDeclaration::ProcedureDeclaration(const Pasc::ProcDeclaration& pd) {
  procHead = std::make_unique<ProcHeading>(pd.prochead());
  if(pd.has_blk())
    blk = std::make_unique<Block>(pd.blk());
  directive = pd.dir();
}

llvm::Value *ProcedureDeclaration::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

//////////////////////////
// FUNCTION HEADING
//////////////////////////
std::string FuncHeading::getName() { return "FUNC_HEAD"; }

FuncHeading::FuncHeading(const Pasc::FuncHeading& fh) {
  name = fh.name();
  for (int i = 0; i < fh.params_size(); ++i) {
    params.push_back(deserializeFormalParam(fh.params(i)));
  }
  retType = deserializeType(fh.returntype());
}

std::vector<llvm::Type*> FuncHeading::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

//////////////////////////
// PROCEDURE HEADING
//////////////////////////
std::string ProcHeading::getName() { return "PROC_HEAD"; }

ProcHeading::ProcHeading(const Pasc::ProcHeading& ph) {
  name = ph.name();
  for (int i = 0; i < ph.params_size(); ++i) {
    params.push_back(deserializeFormalParam(ph.params(i)));
  }

  retType = deserializeType(ph.returntype());
}

std::vector<llvm::Type*> ProcHeading::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

//////////////////////////
// VARIABLE PARAM
//////////////////////////
std::string VariableParam::getName() { return "VAR_PARAM"; }

VariableParam::VariableParam(const Pasc::VariableParam& varParam) {
  for (int i = 0; i < varParam.names_size(); ++i) {
    names.push_back(varParam.names(i));
  }
  type = deserializeType(varParam.type());
}

std::vector<llvm::Type*> VariableParam::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

//////////////////////////
// VALUE PARAMETER
//////////////////////////
std::string ValueParam::getName() { return "VALUE_PARAM"; }

ValueParam::ValueParam(const Pasc::ValueParam& valParam) {
  for (int i = 0; i < valParam.names_size(); ++i) {
    names.push_back(valParam.names(i));
  }
  type = deserializeType(valParam.type());
}

std::vector<llvm::Type*> ValueParam::codegen(IRVisitor &v) {
  return v.codegen(*this);
}