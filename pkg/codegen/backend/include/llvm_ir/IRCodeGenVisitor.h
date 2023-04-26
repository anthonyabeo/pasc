#ifndef IR_CODEGEN_VISITOR_H
#define IR_CODEGEN_VISITOR_H

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "SymbolTable.h"
#include "deserialize/Expr.h"
#include "deserialize/Program.h"
#include "deserialize/IRVisitor.h"

class IRCodegenVisitor : public IRVisitor {
private:
  std::unique_ptr<llvm::LLVMContext> ctx;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

  std::unique_ptr<LLVMSymbolTable> symTable;

public:
  explicit IRCodegenVisitor(std::string&);

  void codegenProgram(const ProgramIR &);
  void codegenBlock(const Block &);
  void dumpLLVMIR();
  std::string dumpLLVMIRToString();

  llvm::Type* getLLVMTypeOf(const Type &);

  // Expressions
  llvm::Value *codegen(const IdentifierIR &) override;
  llvm::Value *codegen(const UIntegerLiteral &) override;
  llvm::Value* codegen(const BinaryExpression&) override;

  // Statements
  llvm::Value *codegen(const AssignStmt &) override;
  llvm::Value *codegen(const ProcedureStatement &) override;
  llvm::Value *codegen(const IfStatement&) override;
};

/// @brief IRCodegenException is a custom exception for code generation
class IRCodegenException : public std::exception {
  std::string errorMessage;

public:
  explicit IRCodegenException(const std::string& msg)
      : errorMessage("IR Codegen Error: " + msg){};
  [[nodiscard]] const char *what() const noexcept override { return errorMessage.c_str(); }
};

#endif // IR_CODEGEN_VISITOR_H