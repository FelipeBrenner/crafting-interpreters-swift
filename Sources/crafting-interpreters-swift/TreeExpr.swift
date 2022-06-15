class TreesExpr {
  class Binary: TreeExpr {
    var left: Any;
    var action: String;
    var right: Any;

    override init() {
      super.init();
    }

    required init(left: Any, action: String, right: Any) {
      self.left = left;
      self.action = action;
      self.right = right;
    }
  };

  class Assign: Binary {
    required init(left: Any, action: String, right: Any) {
      super.init(left: left, action: action, right: right);
    }
  };

  class Unary: TreeExpr {
    var action: String;
    var right: Any;

    override init() {
      super.init();
    }

    required init(action: String, right: Any) {
      self.action = action;
      self.right = right;
    }
  };

  class Literal: TreeExpr {
    var value: Any;

    override init() {
      super.init();
    }

    required init(value: Any) {
      self.value = value;
    }
  };

  class Method: TreeExpr {
    var action: String;
    var params: [Any]

    override init() {
      super.init();
    }

    required init(action: String, params: [Any]) {
      self.action = action;
      self.params = params;
    }
  };

  class Variable: Literal {
    required init(value: Any) {
      super.init(value: value);
    }
  };

  class Grouping: TreeExpr {
    var expr: Any;

    override init() {
      super.init();
    }

    required init(expr: Any) {
      self.expr = expr;
    }
  };

  class TreeExpr {
    func accept(visitor: TreePrinterType) -> String {
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