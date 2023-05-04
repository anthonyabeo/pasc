#ifndef TYPE_H
#define TYPE_H

#include <string>

#include "../program.pb.h"

struct Type {
  virtual ~Type() = default;
  [[nodiscard]] virtual std::string GetName() const = 0;
};

std::unique_ptr<Type> deserializeType(const Pasc::Type &);

struct IntegerType : public Type {
  std::string name;

  explicit IntegerType(const Pasc::Type_Integer &);
  [[nodiscard]] std::string GetName() const override;
};

#endif // TYPE_H