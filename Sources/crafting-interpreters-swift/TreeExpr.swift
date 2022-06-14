class TreesExpr {
  class Binary: TreeExpr {
    init(left: String, action: String, right: String) {
      super.init();
      self.left = left;
      self.action = action;
      self.right = right;
    }
  };

  class Assign: Binary {
    init(left: String, action: String, right: String) {
      super.init(left, action, right);
    }
  };

  class Unary: TreeExpr {
    init(action: String, right: String) {
      super.init();
      self.action = action;
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
    init(action: String, params: [Any]) {
      super.init();
      self.action = action;
      self.params = params;
    }
  };

  class Variable: Literal {
    init(value: String) {
      super.init(value);
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
      if (self is UnaryType) {
        return visitor.visitUnaryTreeExpr(expr: self);
      }
      if (self is GroupingType) {
        return visitor.visitGroupingTreeExpr(expr: self);
      }       
      if (self is LiteralType) {
        return visitor.visitLiteralTreeExpr(expr: self);
      }
      if (self is BinaryType) {
        return visitor.visitBinaryTreeExpr(expr: self);
      }
      if (self is MethodType) {
        return visitor.visitMethodTreeExpr(expr: self);
      }
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