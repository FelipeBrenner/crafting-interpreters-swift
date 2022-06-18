class TreePrinter {
  var expr: TreeExprType;

  init(expr: TreeExprType) {
    self.expr = expr;
  }

  func print() -> String {
    return self.expr.accept(visitor: self);
  }

  func visitBinaryTreeExpr(expr: BinaryType) -> String {
    return self.parenthesise(name: expr.action, exprs: [expr.left, expr.right]);
  }

  func visitGroupingTreeExpr(expr: GroupingType) -> String  {
    return self.parenthesise(name: "group", exprs: [expr.expr]);
  }

  func visitLiteralTreeExpr(expr: LiteralType) -> String {
    if ("\(expr.value)" == "0") {
      return "false";
    }

    return "\(expr.value)";
  }

  func visitUnaryTreeExpr(expr: UnaryType) -> String {
    return self.parenthesise(name: expr.action, exprs: [expr.right]);
  }

  func visitMethodTreeExpr(expr: MethodType) -> String {
    return self.parenthesise(name: expr.action, exprs: expr.params);
  }

  func visitTernaryConditionalTreeExpr(expr: TernaryConditionalType) -> String {
    return self.parenthesise(name: "ternary conditional", exprs: [expr.condition, expr.exprIfTrue, expr.exprIfFalse])
  }

  func parenthesise(name: String, exprs: [Any]) -> String {
    var expressionData = ""
    for expr in exprs {
      expressionData = expressionData + (expr as! TreeExprType).accept(visitor: self) + " "
    }
    return "(\(name) \(expressionData.trimmingCharacters(in: .whitespacesAndNewlines)))";
  }
}

typealias TreePrinterType = TreePrinter