#ifndef IR_VISITOR
#define IR_VISITOR

#include "llvm/IR/Type.h"
#include "llvm/IR/Value.h"

class IRVisitor {
public:
    virtual llvm::Value *codegen(const Identifier& id) = 0;
    virtual llvm::Value *codegen(const UIntegerLiteral &uintlit) = 0;
};

#endif // IR_VISITOR