#ifndef IR_CODEGEN_VISITOR
#define IR_CODEGEN_VISITOR

#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"

#include "Expr.h"
#include "IRVisitor.h"
#include "Program.h"
#include "SymbolTable.h"

class IRCodegenVisitor : public IRVisitor {
protected:
  std::unique_ptr<llvm::LLVMContext> ctx;
  std::unique_ptr<llvm::IRBuilder<>> builder;
  std::unique_ptr<llvm::Module> module;

  std::unique_ptr<LLVMSymbolTable> symTable;

public:
  IRCodegenVisitor();
  ~IRCodegenVisitor() override= default;

  void codegenProgram(const ProgramIR &);
  void codegenBlock(const Block &);
  void dumpLLVMIR();

 llvm::Type* getLLVMTypeOf(const Type &);

  // Expressions
  llvm::Value *codegen(const IdentifierIR &) override;
  llvm::Value *codegen(const UIntegerLiteral &) override;

  // Statements
  llvm::Value *codegen(const AssignStmt &) override;
  llvm::Value *codegen(const ProcedureStatement &) override;
};

/// @brief IRCodegenException is a custom exception for code generation
class IRCodegenException : public std::exception {
  std::string errorMessage;

public:
  explicit IRCodegenException(const std::string& msg)
      : errorMessage("IR Codegen Error: " + msg){};
  [[nodiscard]] const char *what() const noexcept override { return errorMessage.c_str(); }
};

#endif // IR_CODEGEN_VISITOR