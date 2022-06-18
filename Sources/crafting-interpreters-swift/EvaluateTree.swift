import Foundation

class EvaluateTree {
  var state: [String: Any];
  var trees: [TreeExprType];

  init(trees: [TreeExprType]) {
    self.state = [:];
    self.trees = trees;
  }

  func start() -> Any? {
    var lastLine: Any?;
    for tree in self.trees {
      lastLine = self.evaluate(operation: tree);
    }
    return lastLine;
  }

  func evaluate(operation: TreeExprType) -> Any? {
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
    let params = operation.params.map { self.evaluate(operation: $0 as! TreeExprType) };
    return methodMapping(method: operation.action, params: params)!;
  }

  func assignEvaluation(operation: AssignType) {
    let rightHandValue = self.evaluate(operation: operation.right as! TreeExprType);
    self.state[(operation.left as! VariableType).value as! String] = rightHandValue;
  }

  func groupingEvaluation(operation: GroupingType) -> Any? {
    return self.evaluate(operation: operation.expr as! TreeExprType);
  }

  func binaryEvaluation(operation: BinaryType) -> Any? {
    let leftHandValue = self.evaluate(operation: operation.left as! TreeExprType);
    let rightHandValue = self.evaluate(operation: operation.right as! TreeExprType);

    switch operation.action {
      case TokenEnum.LESS_THAN.final:
        return (leftHandValue as! String) < (rightHandValue as! String);
      case TokenEnum.GREATER_THAN.final:
        return (leftHandValue as! String) > (rightHandValue as! String);
      case TokenEnum.EQUAL.final:
        return (leftHandValue as! String) == (rightHandValue as! String);
      case TokenEnum.NOT_EQUAL.final:
        return (leftHandValue as! String) != (rightHandValue as! String);
      case TokenEnum.GREATER_THAN_OR_EQUAL.final:
        return (leftHandValue as! String) >= (rightHandValue as! String);
      case TokenEnum.LESS_THAN_OR_EQUAL.final:
        return (leftHandValue as! String) <= (rightHandValue as! String);
      case TokenEnum.EXPONENT.final:
        return pow((leftHandValue as! Double), (rightHandValue as! Double));
      case TokenEnum.MULTIPLY.final:
        return (leftHandValue as! Double) * (rightHandValue as! Double);
      case TokenEnum.DIVIDE.final:
        return (leftHandValue as! Double) / (rightHandValue as! Double);
      case TokenEnum.PLUS.final:
        return (leftHandValue as! Double) + (rightHandValue as! Double);
      case TokenEnum.MINUS.final:
        return (leftHandValue as! Double) - (rightHandValue as! Double);
      case TokenEnum.AND.final:
        return (leftHandValue as! Bool) && (rightHandValue as! Bool);
      case TokenEnum.OR.final:
        return (leftHandValue as! Bool)  || (rightHandValue as! Bool);
      case TokenEnum.XOR.final:
        return ((leftHandValue as! Bool) != (rightHandValue as! Bool)) == true;
      default:
        return nil;
    }
  }

  func unaryEvaluation(operation: UnaryType) -> Any? {
    let rightHandValue = self.evaluate(operation: operation.right as! TreeExprType);
    switch operation.action {
      case TokenEnum.NOT.final:
        return rightHandValue == nil;
      case TokenEnum.MINUS.final:
        return -(rightHandValue as! Double);
      default:
        return nil
    }
  }
}
