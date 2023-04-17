#include "program.pb.h"

#include "Expr.h"
#include "Deserializer.h"

#include <memory>
#include "IRVisitor.h"

/// @brief
/// @param expr
/// @return std::unique_ptr<Expr> a pointer to an expression type
std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression &expr) {
  switch (expr.expr_case()) {
  case Pasc::Expression::kId:
    return std::make_unique<IdentifierIR>(expr);
  case Pasc::Expression::kUint:
    return std::make_unique<UIntegerLiteral>(expr.uint());
  default:
    throw DeserializeProtobufException("invalid case");
  }
}

/// @brief
/// @param uintlit
UIntegerLiteral::UIntegerLiteral(const Pasc::UIntLiteral &uintlit) {
  value = uintlit.value();
}

/// @brief
/// @param v
/// @return
llvm::Value *UIntegerLiteral::codegen(IRVisitor &v) { return v.codegen(*this); }

/// @brief
/// @param id
IdentifierIR::IdentifierIR(const Pasc::Expression &expr) { name = expr.id().name(); }

/// @brief
/// @param v
/// @return
llvm::Value *IdentifierIR::codegen(IRVisitor &v) { return v.codegen(*this); }