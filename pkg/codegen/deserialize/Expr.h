#ifndef EXPR_H
#define EXPR_H

#include <string>

#include "program.pb.h"

struct Expr {
    virtual ~Expr() = default;
};

std::unique_ptr<Expr> deserializeExpr(const Pasc::Expression&);

/// @brief Identifier denotes a user-defined, non-keyword symbol
struct Identifier : public Expr {
    std::string name;

    Identifier(const Pasc::Identifier&);
};

/// @brief UIntegerLiteral denoted an unsigned 32-bit integer literal value.
struct UIntegerLiteral : public Expr {
    int value;

    UIntegerLiteral(const Pasc::UIntLiteral&);
};

#endif // EXPR_H