#include "Type.h"
#include "program.pb.h"

std::unique_ptr<Type> deserializeType(const Pasc::Type& t) {
    switch (t.type_case()) {
    case Pasc::Type::kInt:
        return std::unique_ptr<IntegerType>(new IntegerType(t.int_()));
        break;
    
    default:
        return nullptr;
    }
}

IntegerType::IntegerType(const Pasc::Integer& i) {
    name = i.name();
}