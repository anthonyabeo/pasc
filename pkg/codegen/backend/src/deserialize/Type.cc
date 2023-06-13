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
  case Pasc::Type_TypeKind_REAL:
    return std::make_unique<RealType>(t.real());
  case Pasc::Type_TypeKind_STR:
    return std::make_unique<StringType>(t.str());
  case Pasc::Type_TypeKind_ENUM:
    return std::make_unique<EnumType>(t.en());
  case Pasc::Type_TypeKind_SUB_RANGE:
    return std::make_unique<SubRangeType>(t.subr());
  case Pasc::Type_TypeKind_ARRAY:
    return std::make_unique<ArrayType>(t.arr());
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

///////////////////////
// REAL
///////////////////////
RealType::RealType(const Pasc::Type_Real &rt){
  name = rt.name();
}

std::string RealType::GetName() const { return name; }

llvm::Type *RealType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

///////////////////////
// STRING
///////////////////////
StringType::StringType(const Pasc::Type_String &st) {
  name = st.name();
}

std::string StringType::GetName() const { return name; }

llvm::Type *StringType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

///////////////////////
// ENUM
///////////////////////
EnumType::EnumType(const Pasc::Type_Enum &en) {
  name = en.name();
  for (int i = 0; i < en.elems_size(); ++i) {
    elems.push_back(en.elems(i));
  }
}

std::string EnumType::GetName() const { return name; }

llvm::Type *EnumType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

///////////////////////
// SUBRANGE
///////////////////////
SubRangeType::SubRangeType(const Pasc::Type_SubRange &srt) {
  name = srt.name();
  start = srt.start();
  end = srt.end();
  host_type = deserializeType(srt.hosttype());
}

llvm::Type *SubRangeType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string SubRangeType::GetName() const { return name; }

///////////////////////
// ARRAY
///////////////////////
ArrayType::ArrayType(const Pasc::Type_Array &arr) {
  name = arr.name();
  comp_type = deserializeType(arr.comptype());
  for (int i = 0; i < arr.indices_size(); ++i) {
    indices.push_back(deserializeType(arr.indices(i)));
  }
}

llvm::Type *ArrayType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}

std::string ArrayType::GetName() const { return name; }

///////////////////////
// CHAR
///////////////////////
CharType::CharType(const Pasc::Type_Char &ct) {
  name = ct.name();
}

std::string CharType::GetName() const { return name; }

llvm::Type *CharType::codegen(IRVisitor &v) {
  return v.codegen(*this);
}