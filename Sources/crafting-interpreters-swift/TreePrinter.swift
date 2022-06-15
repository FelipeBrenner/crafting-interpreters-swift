class TreePrinter {
  var expr: TreeExprType;

  init(expr: TreeExprType) {
    self.expr = expr;
  }

  func print()  -> String {
    return self.expr.accept(visitor: self);
  }

  func visitBinaryTreeExpr(expr: BinaryType) -> String {
    return self.parenthesise(name: expr.action, BinaryType(left: expr.left, action: expr.action, right: expr.right));
  }

  func visitGroupingTreeExpr(expr: GroupingType) -> String  {
    return self.parenthesise(name: "group", GroupingType(expr: expr.expr));
  }

  func visitLiteralTreeExpr(expr: LiteralType) -> String {
    if ("\(expr.value)" == "0") {
      return "false";
    }

    return "\(expr.value)";
  }

  func visitUnaryTreeExpr(expr: UnaryType) -> String {
    return self.parenthesise(name: expr.action, UnaryType(action: expr.action, right: expr.right));
  }

  func visitMethodTreeExpr(expr: MethodType) -> String {
    return self.parenthesise(name: expr.action, ...expr.params);
  }

  func parenthesise(name: String, _ exprs: TreeExprType...) -> String {
    let expressionData = exprs.map{ "\($0.accept(visitor: self)) " };
    return "(\(name) \(expressionData))";
  }
}

typealias TreePrinterType = TreePrinter