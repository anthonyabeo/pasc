#include "program.pb.h"

#include "Expr.h"

std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression& expr) {
    switch (expr.expr_case()) {
    case Pasc::Expression::kId:
        return std::unique_ptr<Identifier>(new Identifier(expr.id()));
        break;
    case Pasc::Expression::kUint:
        return std::unique_ptr<UIntegerLiteral>(new UIntegerLiteral(expr.uint()));
        break;
    default:
        return nullptr;
    }
}

UIntegerLiteral::UIntegerLiteral(const Pasc::UIntLiteral& uintlit) {
    value = uintlit.value();
}


Identifier::Identifier(const Pasc::Identifier& id) {
    name = id.name();
}

