#include "program.pb.h"

#include "Statement.h"
#include "Expr.h"

std::unique_ptr<Statement> deserializeStmt(const Pasc::Statement& stmt) {
    switch (stmt.stmt_case()) {
    case Pasc::Statement::kAssignStmt:
        return std::unique_ptr<AssignStmt>(new AssignStmt(stmt.assignstmt()));
        break;
    case Pasc::Statement::kProcStmt:
        return std::unique_ptr<ProcedureStatement>(
                    new ProcedureStatement(stmt.procstmt()));
        break;
    default:
        return nullptr;
    }
}

AssignStmt::AssignStmt(const Pasc::AssignStmt& stmt) {
    variable = std::unique_ptr<Identifier>(new Identifier(stmt.variable()));
    value = deserializeExpr(stmt.value());
}

ProcedureStatement::ProcedureStatement(const Pasc::ProcedureStmt& stmt) {
    name = std::unique_ptr<Identifier>(new Identifier(stmt.name()));
    for (size_t i = 0; i < stmt.args_size(); i++) {
        params.push_back(deserializeExpr(stmt.args(i)));
    }
}