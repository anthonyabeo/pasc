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
  virtual llvm::Type *codegen(IRVisitor &) = 0;
};

std::unique_ptr<Type> deserializeType(const Pasc::Type &);

struct IntegerType : public Type {
  std::string name;

  explicit IntegerType(const Pasc::Type_Integer &);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

struct BoolType : public Type {
  std::string name;

  explicit BoolType(const Pasc::Type_Boolean&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

struct VoidType : public Type {
  std::string name;

  explicit VoidType(const Pasc::Type_Void&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

struct RealType : public Type {
  std::string name;

  explicit RealType(const Pasc::Type_Real&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

struct StringType : public Type {
  std::string name;

  explicit StringType(const Pasc::Type_String&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

struct EnumType : public Type {
  std::string name;
  std::vector<std::string> elems;

  explicit EnumType(const Pasc::Type_Enum&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

struct SubRangeType : public Type {
  std::string name;
  int32_t start;
  int32_t end;
  std::unique_ptr<Type> host_type;

  explicit SubRangeType(const Pasc::Type_SubRange&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

struct ArrayType : public Type {
  std::string name;
  std::vector<std::unique_ptr<Type>> indices;
  std::unique_ptr<Type> comp_type;

  explicit ArrayType(const Pasc::Type_Array&);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

struct CharType : public Type {
  std::string name;

  explicit CharType(const Pasc::Type_Char &);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};


struct RecordType : public Type {
  std::string name;
  std::map<std::string, std::unique_ptr<Type>> fields;

  explicit RecordType(const Pasc::Type_Record &);
  [[nodiscard]] std::string GetName() const override;
  llvm::Type *codegen(IRVisitor &) override;
};

#endif // TYPE_H