#include <memory>

#include "program.pb.h"

#include "deserialize/Deserializer.h"
#include "deserialize/Type.h"

std::unique_ptr<Type> deserializeType(const Pasc::Type &t) {
  switch (t.tk()) {
  case Pasc::Type_TypeKind_INTEGER:
    return std::make_unique<IntegerType>(t.int_());
  default:
    throw DeserializeProtobufException("invalid case");
  }
}

IntegerType::IntegerType(const Pasc::Type_Integer &i) { name = i.name(); }

std::string IntegerType::GetName() const { return name; }