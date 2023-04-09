#include <stdlib.h>
#include <string>

#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/IR/Verifier.h"

#include "Deserializer.h"
#include "IRCodeGenVisitor.h"

int main(int argc, char **argv) {
  std::string filePath(argv[1]);
  auto programIR =
      CreateInternalIRFromProtobuf(DeserialiseProtobufFile(filePath));
  try {
    IRCodegenVisitor codeGen;
    codeGen.codegenProgram(*programIR);
  } catch (IRCodegenException *ex) {
    std::cerr << ex->what();
    return 1;
  }

  // std::cout << "Program Name: " << programIR->name << std::endl;
  // std::cout << "Program Params: ";
  // for (size_t i = 0; i < programIR->params.size(); i++) {
  //   std::cout << programIR->params.at(i);
  //   if (i != programIR->params.size() - 1) {
  //     std::cout << ", ";
  //   }
  // }
  // std::cout << std::endl;

  return 0;
}