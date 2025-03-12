#include <iostream>

#include "expr.h"
#include "ast_pretty_printer.h"
#include "type_inference.h"

int main() {
  auto ast = std::make_shared<BlockStmt>(
    std::vector<std::shared_ptr<Stmt>>{
      std::make_shared<FunctionStmt>(
        Var(0),
        std::vector{ Var(1, std::make_shared<IntegerType>()), Var(2, std::make_shared<IntegerType>()) },
        std::make_shared<IntegerType>(),
        std::make_shared<BlockStmt>(
          std::vector<std::shared_ptr<Stmt>>{
            std::make_shared<DeclareStmt>( // var 3 = var 1 + int 3
              Var(3),
              std::make_shared<BinaryExpr>(
                std::make_shared<VariableExpr>(Var(1)),
                BinaryOperator::Add,
                std::make_shared<IntegerExpr>("3")
              )
            ),
            std::make_shared<DeclareStmt>( // var 4 = var 2 + var 3
              Var(4),
              std::make_shared<BinaryExpr>(
                std::make_shared<VariableExpr>(Var(2)),
                BinaryOperator::Add,
                std::make_shared<VariableExpr>(Var(3))
              )
            ),
            std::make_shared<DeclareStmt>( // var 4 = var 2 + var 3
              Var(10),
              std::make_shared<UnaryExpr>(UnaryOperator::Not , std::make_shared<BoolExpr>(true))
            ),
            std::make_shared<IfStmt>(
              // std::make_shared<IntegerExpr>("3"), // this should be a bool
              // std::make_shared<UnaryExpr>(UnaryOperator::Not , std::make_shared<BoolExpr>(true)),
                // std::make_shared<UnaryExpr>(UnaryOperator::Negate , std::make_shared<IntegerExpr>("3")),
                std::make_shared<UnaryExpr>(UnaryOperator::Not, std::make_shared<VariableExpr>(Var(10))),
                std::make_shared<BlockStmt>(
                  std::vector<std::shared_ptr<Stmt>>{
                    std::make_shared<DeclareStmt>(Var(4), std::make_shared<IntegerExpr>("3")), // shadowing
                    // std::make_shared<ReturnStmt>(std::make_shared<VariableNode>(Var(4)))
                  }
                ),
                std::make_shared<BlockStmt>(
                  std::vector<std::shared_ptr<Stmt>>{
                    std::make_shared<AssignStmt>(Var(4), std::make_shared<IntegerExpr>("3")),
                    // std::make_shared<AssignStmt>(Var(4), std::make_shared<BoolExpr>(true)),
                    // std::make_shared<AssignStmt>(Var(6), std::make_shared<DoubleExpr>("3")),
                    // std::make_shared<ReturnStmt>(std::make_shared<VariableNode>(Var(4)))
                  }
                )
            ),
            std::make_shared<ReturnStmt>(std::make_shared<VariableExpr>(Var(4)))
          }
        )
      )
    }
  );

  ASTPrettyPrinter printer(nullptr);
  printer.print(*ast);

  TypeInference inference;
  inference.perform(*ast);

  return 0;
}