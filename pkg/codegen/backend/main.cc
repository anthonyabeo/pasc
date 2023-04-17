#include <string>

#include "include/deserialize/Deserializer.h"
#include "include/llvm_ir/IRCodeGenVisitor.h"

int main(int argc, char **argv) {
  std::string filePath(argv[1]);
  auto f = DeserializeProtobufFile(filePath);
  auto programIR= CreateInternalIRFromProtobuf(f);
  try {
    IRCodegenVisitor codeGen;
    codeGen.codegenProgram(*programIR);
    codeGen.dumpLLVMIR();
  } catch (IRCodegenException *ex) {
    std::cerr << ex->what();
    return 1;
  }

  return 0;
}