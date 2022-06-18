import Foundation

class EvaluateTree {
  var state: [String: Any];
  var trees: [TreeExprType];

  init(trees: [TreeExprType]) {
    self.state = [:];
    self.trees = trees;
  }

  func start() {
    for tree in self.trees {
      _ = self.evaluate(operation: tree);
    }
  }

  func evaluate(operation: TreeExprType) -> Any? {
    switch true {
      case operation is MethodType:
        return self.methodEvaluation(operation: operation as! MethodType)
      case operation is VariableType:
        let operation = operation as! VariableType
        if self.state[operation.value as! String] == nil {
          print("Variable does not exist on this scope.");
          return nil
        }
        return self.state[operation.value as! String]!;
      case operation is LiteralType:
        return (operation as! LiteralType).value;
      case operation is UnaryType:
        return self.unaryEvaluation(operation: operation as! UnaryType);
      case operation is AssignType:
        return self.assignEvaluation(operation: operation as! AssignType);
      case operation is BinaryType:
        return self.binaryEvaluation(operation: operation as! BinaryType);
      case operation is GroupingType:
        return self.groupingEvaluation(operation: operation as! GroupingType);
      case operation is TernaryConditionalType:
        return self.ternaryConditionalEvaluation(operation: operation as! TernaryConditionalType);
      default:
        return nil;
    }
  }

  func methodEvaluation(operation: MethodType) -> Any? {
    let params = operation.params.map { self.evaluate(operation: $0 as! TreeExprType) };
    return methodMapping(method: operation.action, params: params);
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
        return (leftHandValue as! Double) < (rightHandValue as! Double);
      case TokenEnum.GREATER_THAN.final:
        return (leftHandValue as! Double) > (rightHandValue as! Double);
      case TokenEnum.EQUAL.final:
        return (leftHandValue as! Double) == (rightHandValue as! Double);
      case TokenEnum.NOT_EQUAL.final:
        return (leftHandValue as! Double) != (rightHandValue as! Double);
      case TokenEnum.GREATER_THAN_OR_EQUAL.final:
        return (leftHandValue as! Double) >= (rightHandValue as! Double);
      case TokenEnum.LESS_THAN_OR_EQUAL.final:
        return (leftHandValue as! Double) <= (rightHandValue as! Double);
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

  func ternaryConditionalEvaluation(operation: TernaryConditionalType) -> Any? {
    let conditionHandValue = self.evaluate(operation: operation.condition as! TreeExprType) as! Bool;
    let exprIfTrueHandValue = self.evaluate(operation: operation.exprIfTrue as! TreeExprType);
    let exprIfFalseHandValue = self.evaluate(operation: operation.exprIfFalse as! TreeExprType);

    return conditionHandValue ? exprIfTrueHandValue : exprIfFalseHandValue;
  }
}
