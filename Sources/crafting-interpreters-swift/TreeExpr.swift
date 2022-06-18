class TreesExpr {
  class Binary: TreeExpr {
    var left: Any;
    var action: String;
    var right: Any;

    required init(left: Any, action: String, right: Any) {
      self.left = left;
      self.action = action;
      self.right = right;
    }

    override var description: String {
      get {
          return "Binary { left: \(left), action: \(action), right: \(right) }"
      }
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

    required init(action: String, right: Any) {
      self.action = action;
      self.right = right;
    }
  };

  class Literal: TreeExpr {
    var value: Any;

    required init(value: Any) {
      self.value = value;
    }
  };

  class Method: TreeExpr {
    var action: String;
    var params: [Any]

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

    required init(expr: Any) {
      self.expr = expr;
    }
  };

  class TernaryConditional: TreeExpr {
    var condition: Any;
    var exprIfTrue: Any;
    var exprIfFalse: Any;

    required init(condition: Any, exprIfTrue: Any, exprIfFalse: Any) {
      self.condition = condition;
      self.exprIfTrue = exprIfTrue;
      self.exprIfFalse = exprIfFalse;
    }
  }

  class TreeExpr {
    func accept(visitor: TreePrinterType) -> String {
      if (self is UnaryType) {
        return visitor.visitUnaryTreeExpr(expr: self as! UnaryType);
      }
      if (self is GroupingType) {
        return visitor.visitGroupingTreeExpr(expr: self as! GroupingType);
      }       
      if (self is LiteralType) {
        return visitor.visitLiteralTreeExpr(expr: self as! LiteralType);
      }
      if (self is BinaryType) {
        return visitor.visitBinaryTreeExpr(expr: self as! BinaryType);
      }
      if (self is MethodType) {
        return visitor.visitMethodTreeExpr(expr: self as! MethodType);
      }
       if (self is TernaryConditional) {
        return visitor.visitTernaryConditionalTreeExpr(expr: self as! TernaryConditional);
      }
      
      return ""
    }

    var description: String {
      get {
          return ""
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
typealias TernaryConditionalType = TreesExpr.TernaryConditional