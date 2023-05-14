#include <memory>

#include "proto/program.pb.h"

#include "deserialize/Deserializer.h"
#include "deserialize/Type.h"
#include "deserialize/IRVisitor.h"

std::unique_ptr<Type> deserializeType(const Pasc::Type &t) {
  switch (t.tk()) {
  case Pasc::Type_TypeKind_INTEGER:
    return std::make_unique<IntegerType>(t.int_());
  case Pasc::Type_TypeKind_BOOLEAN:
    return std::make_unique<BoolType>(t.bool_());
  case Pasc::Type_TypeKind_VOID:
    return std::make_unique<VoidType>(t.void_());
  default:
    throw DeserializeProtobufException("invalid case");
  }
}

///////////////////////
// INTEGER
///////////////////////
IntegerType::IntegerType(const Pasc::Type_Integer &i) { name = i.name(); }

std::string IntegerType::GetName() const { return name; }

llvm::Type *IntegerType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

///////////////////////
// BOOLEAN
///////////////////////
BoolType::BoolType(const Pasc::Type_Boolean &b) {
  name = b.name();
}

std::string BoolType::GetName() const { return name; }

llvm::Type *BoolType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

///////////////////////
// VOID
///////////////////////
VoidType::VoidType(const Pasc::Type_Void &v) {
  name = v.name();
}

std::string VoidType::GetName() const { return name; }

llvm::Type *VoidType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}