class TreesExpr {
  class Binary: TreeExpr {
    init(left: String, operator: String, right: String) {
      super.init();
      self.left = left;
      self.operator = operator;
      self.right = right;
    }
  };

  class Assign: self.Binary {
    init(left: String, operator: String, right: String) {
      super(left, operator, right);
    }
  };

  class Unary: TreeExpr {
    init(operator: String, right: String) {
      super.init();
      self.operator = operator;
      self.right = right;
    }
  };

  class Literal: TreeExpr {
    init(value: Any) {
      super.init();
      self.value = value;
    }
  };

  class Method: TreeExpr {
    init(operator: String, params: [Any]) {
      super.init();
      self.operator = operator;
      self.params = params;
    }
  };

  class Variable: self.Literal {
    init(value: String) {
      super(value);
    }
  };

  class Grouping: TreeExpr {
    init(expr: String) {
      super.init();
      self.expr = expr;
    }
  };

  class TreeExpr {
    func accept(visitor: TreePrinterType) {
      if (self instanceof TreeExpr.Unary) return visitor.visitUnaryTreeExpr(expr: self);
      if (self instanceof TreeExpr.Grouping)
        return visitor.visitGroupingTreeExpr(expr: self);
      if (self instanceof TreeExpr.Literal)
        return visitor.visitLiteralTreeExpr(expr: self);
      if (self instanceof TreeExpr.Binary)
        return visitor.visitBinaryTreeExpr(expr: self);
      if (self instanceof TreeExpr.Method)
        return visitor.visitMethodTreeExpr(expr: self);
    }
  }
}

let TreeExpr = TreesExpr();
typealias TreeExprType = TreesExpr.TreeExpr
typealias BinaryType = TreesExpr.Binary
typealias AssignType = TreesExpr.Assign
typealias UnaryType = TreesExpr.Unary
typealias LiteralType = TreesExpr.Literal
typealias MethodType = TreesExpr.Method
typealias VariableType = TreesExpr.Variable
typealias GroupingType = TreesExpr.Grouping