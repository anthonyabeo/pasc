#ifndef TYPE_H
#define TYPE_H

#include <string>

#include "program.pb.h"

struct Type {
    virtual ~Type() = default;
};

std::unique_ptr<Type> deserializeType(const Pasc::Type&);


struct IntegerType : public Type {
    std::string name;

    IntegerType(const Pasc::Integer&);
};

#endif // TYPE_H