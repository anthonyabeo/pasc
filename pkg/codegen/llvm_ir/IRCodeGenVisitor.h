#ifndef IR_CODEGEN_VISITOR
#define IR_CODEGEN_VISITOR

#include "IRVisitor.h"

class IRCodegenVisitor: public IRVisitor {
public:
    IRCodegenVisitor();

    virtual llvm::Value *codegen(const Identifier&) = 0;
    virtual llvm::Value *codegen(const UIntegerLiteral&) = 0;
};


#endif // IR_CODEGEN_VISITOR