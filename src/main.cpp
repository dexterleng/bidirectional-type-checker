#include <iostream>

#include "expr.h"
#include "ast_pretty_printer.h"
#include "type_inference.h"

int main() {
  // // Variable representations
  // Var x(0); // x variable (first parameter)
  // Var y(1); // y variable (second parameter)
  // Var result(2);
  //
  // // Create variable nodes for x and y
  // auto xVarNode = std::make_shared<VariableNode>(x);
  // auto yVarNode = std::make_shared<VariableNode>(y);
  //
  // // Create the addition node x + y (body of the inner lambda)
  // auto addNode = std::make_shared<AddNode>(xVarNode, yVarNode);
  //
  // // Create the inner lambda: y -> x + y
  // auto innerLambda = std::make_shared<Function>(y, addNode);
  //
  // // Create the outer lambda: x -> (y -> x + y)
  // auto outerLambda = std::make_shared<FunctionNode>(x, innerLambda);
  //
  // // Create the arguments
  // auto arg1 = std::make_shared<DoubleNode>("3.0");
  // auto arg2 = std::make_shared<DoubleNode>("4.1");
  //
  // // Apply the function to the first argument: (x -> y -> x + y)(3.0)
  // auto firstApply = std::make_shared<ApplyNode>(outerLambda, arg1);
  //
  // // Apply the result to the second argument: ((x -> y -> x + y)(3.0))(4.1)
  // auto secondApply = std::make_shared<ApplyNode>(firstApply, arg2);
  //
  // // Create an assignment statement: result = ((x -> y -> x + y)(3.0))(4.1)
  // auto assignStmt = std::make_shared<AssignStmt>(result, secondApply);
  //
  // // Create a block containing the assignment
  // std::vector<std::shared_ptr<Stmt>> statements;
  // statements.push_back(assignStmt);
  // auto blockStmt = std::make_shared<BlockStmt>(statements);
  //
  // TypeInference inference;
  // inference.perform(*secondApply);
  //
  // // Pretty print the resulting AST
  // ASTPrettyPrinter printer(nullptr);
  // std::cout << "Expression wrapped in a block with assignment: " << std::endl;
  // std::cout << "{ result = (x -> y -> x + y)(3.0)(4.1) }" << std::endl;
  // std::cout << "AST:" << std::endl;
  // printer.printStatement(*blockStmt);

  auto ast = std::make_shared<BlockStmt>(
    std::vector<std::shared_ptr<Stmt>>{
      std::make_shared<FunctionStmt>(
        Var(0),
        std::vector{ Var(1, std::make_shared<IntegerType>()), Var(2, std::make_shared<IntegerType>()) },
        std::make_shared<IntegerType>(),
        std::make_shared<BlockStmt>(
          std::vector<std::shared_ptr<Stmt>>{
            std::make_shared<AssignStmt>(
              Var(3),
              std::make_shared<AddNode>(
                std::make_shared<VariableNode>(Var(1)),
                std::make_shared<VariableNode>(Var(2))
              )
            ),
            std::make_shared<AssignStmt>(Var(4), std::make_shared<IntegerNode>("3")),
            std::make_shared<ReturnStmt>(std::make_shared<VariableNode>(Var(4)))
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