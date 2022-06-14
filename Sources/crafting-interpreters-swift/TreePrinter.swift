class TreePrinter {
  init(expr: TreeExprType) {
    self.expr = expr;
  }

  func print() {
    return self.expr.accept(visitor: self);
  }

  func visitBinaryTreeExpr(expr: BinaryType) {
    return self.parenthesise(name: expr.action, expr.left, expr.right);
  }

  func visitGroupingTreeExpr(expr: GroupingType) {
    return self.parenthesise(name: "group", expr.expr);
  }

  func visitLiteralTreeExpr(expr: LiteralType) {
    if (expr.value == false) {
      return "false";
    }
    if (!expr.value) {
      return "nil";
    }  
    return expr.value.toString();
  }

  func visitUnaryTreeExpr(expr: UnaryType) {
    return self.parenthesise(name: expr.action, expr.right);
  }

  func visitMethodTreeExpr(expr: MethodType) {
    return self.parenthesise(name: expr.action, ...expr.params);
  }

  func parenthesise(name: String, exprs: TreeExprType...) {
    var expressionData = exprs.map((expr) => expr.accept(self)).join(" ");
    var expressionData = exprs.map{ $0.accept(self)).join(" ") };
    return "(\(name) \(expressionData))";
  }
}

typealias TreePrinterType = TreePrinter