import Foundation

class EvaluateTree {
  var state: [String: Any];
  var trees: [TreeExprType];

  init(trees: [TreeExprType]) {
    self.state = [:];
    self.trees = trees;
  }

  func start() -> TreesExpr.TreeExpr? {
    var lastLine: TreesExpr.TreeExpr?;
    for tree in self.trees {
      lastLine = self.evaluate(operation: tree);
    }
    return lastLine;
  }

  func evaluate(operation: TreeExprType) -> TreeExprType? {
    switch true {
      case operation is MethodType:
        return self.methodEvaluation(operation: operation as! MethodType) as! TreeExprType?
      case operation is VariableType:
        let operation = operation as! VariableType
        if self.state[operation.value as! String] != nil {
          print("Variable does not exist on this scope.");
          return nil
        }
        return self.state[operation.value as! String]! as! TreeExprType?;
      case operation is LiteralType:
        let op = operation as! LiteralType
        return op.value as! TreeExprType?;
      case operation is UnaryType:
        return self.unaryEvaluation(operation: operation as! UnaryType) as! TreeExprType?;
      case operation is AssignType:
        return self.assignEvaluation(operation: operation as! AssignType) as! TreeExprType?;
      case operation is BinaryType:
        return self.binaryEvaluation(operation: operation as! BinaryType) as! TreeExprType?;
      case operation is GroupingType:
        return self.groupingEvaluation(operation: operation as! GroupingType) as! TreeExprType?;
      default:
        return nil;
    }
  }

  func methodEvaluation(operation: MethodType) -> Any? {
    let params = operation.params.map { self.evaluate(operation: $0) };
    return methodMapping(method: operation.action, params: params)!;
  }

  func assignEvaluation(operation: AssignType) {
    let rightHandValue = self.evaluate(operation: operation.right);
    self.state[operation.left.value] = rightHandValue;
  }

  func groupingEvaluation(operation: GroupingType) -> Any {
    return self.evaluate(operation: operation.expr);
  }

  func binaryEvaluation(operation: BinaryType) -> Any {
    let leftHandValue = self.evaluate(operation: operation.left);
    let rightHandValue = self.evaluate(operation: operation.right);

    switch operation.operator {
      case TokenEnum.LESS_THAN.final:
        return leftHandValue < rightHandValue;
      case TokenEnum.GREATER_THAN.final:
        return leftHandValue > rightHandValue;
      case TokenEnum.EQUAL.final:
        return leftHandValue == rightHandValue;
      case TokenEnum.NOT_EQUAL.final:
        return leftHandValue != rightHandValue;
      case TokenEnum.GREATER_THAN_OR_EQUAL.final:
        return leftHandValue >= rightHandValue;
      case TokenEnum.LESS_THAN_OR_EQUAL.final:
        return leftHandValue <= rightHandValue;
      case TokenEnum.EXPONENT.final:
        return Math.pow(leftHandValue, rightHandValue);
      case TokenEnum.MULTIPLY.final:
        return leftHandValue * rightHandValue;
      case TokenEnum.DIVIDE.final:
        return leftHandValue / rightHandValue;
      case TokenEnum.PLUS.final:
        return leftHandValue + rightHandValue;
      case TokenEnum.MINUS.final:
        return leftHandValue - rightHandValue;
      case TokenEnum.AND.final:
        return leftHandValue && rightHandValue;
      case TokenEnum.OR.final:
        return leftHandValue || rightHandValue;
      case TokenEnum.XOR.final:
        return (leftHandValue ^ rightHandValue) == 1;
    }
  }

  func unaryEvaluation(operation: UnaryType) -> Any? {
    let rightHandValue = self.evaluate(operation: operation.right);
    switch operation.operator {
      case TokenEnum.NOT.final:
        return !rightHandValue;
      case TokenEnum.MINUS.final:
        return -rightHandValue;
      default:
        return nil
    }
  }
}
