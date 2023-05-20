#ifndef TYPE_H
#define TYPE_H

#include <string>

#include "../proto/program.pb.h"
#include "../proto/type.pb.h"
#include "llvm/IR/Type.h"

class IRVisitor;

struct Type {
  virtual ~Type() = default;
  [[nodiscard]] virtual std::string GetName() const = 0;
  virtual llvm::Type *codegen(IRVisitor &visitor) = 0;
};

std::unique_ptr<Type> deserializeType(const Pasc::Type &);

struct IntegerType : public Type {
  std::string name;

  explicit IntegerType(const Pasc::Type_Integer &);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &visitor) override;
};

struct BoolType : public Type {
  std::string name;

  explicit BoolType(const Pasc::Type_Boolean&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &visitor) override;
};

struct VoidType : public Type {
  std::string name;

  explicit VoidType(const Pasc::Type_Void&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &visitor) override;
};

struct RealType : public Type {
  std::string name;

  explicit RealType(const Pasc::Type_Real&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &visitor) override;
};

#endif // TYPE_H