class CrawlTokens {
  var tokens: [TokenType]
  var currentTokenIndex: Int

  init(tokens: [TokenType]) {
    self.tokens = tokens;
    self.currentTokenIndex = 0;
  }

  func crawl() async -> TreeExprType {
    return self.expression();
  }

  func expression() -> TreeExprType {
    return self.assign();
  }

  // Binary expressions

  func assign() -> TreeExprType {
    var expr: TreeExprType = self.logical();

    while (self.matchPattern(types: [TokenEnum.ASSIGN])) {
      let previousToken = self.previousToken();
      let right = self.logical();
      expr = AssignType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func logical() -> TreeExprType {
    var expr: TreeExprType = self.equality();

    while (self.matchPattern(types:[TokenEnum.AND, TokenEnum.OR, TokenEnum.XOR])) {
      let previousToken = self.previousToken()
      let right = self.equality()
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func equality() -> TreeExprType {
    var expr: TreeExprType = self.comparation();

    while (self.matchPattern(types:[TokenEnum.NOT_EQUAL, TokenEnum.EQUAL])) {
      let previousToken = self.previousToken();
      let right = self.comparation();
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }
    
    return expr;
  }

  func comparation() -> TreeExprType {
    var expr: TreeExprType = self.additionSubtraction();

    while (
      self.matchPattern(types:[
        TokenEnum.GREATER_THAN,
        TokenEnum.GREATER_THAN_OR_EQUAL,
        TokenEnum.LESS_THAN,
        TokenEnum.LESS_THAN_OR_EQUAL]
      )
    ) {
      let previousToken = self.previousToken();
      let right = self.additionSubtraction();
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func additionSubtraction() -> TreeExprType {
    var expr: TreeExprType = self.multiplicationDivision();

    while (self.matchPattern(types:[TokenEnum.PLUS, TokenEnum.MINUS])) {
      let previousToken = self.previousToken();
      let right = self.multiplicationDivision();
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func multiplicationDivision() -> TreeExprType {
    var expr: TreeExprType = self.potentiation();

    while (self.matchPattern(types:[TokenEnum.MULTIPLY, TokenEnum.DIVIDE])) {
      let previousToken = self.previousToken();
      let right = self.potentiation();
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func potentiation() -> TreeExprType {
    var expr: TreeExprType = self.unary();

    while (self.matchPattern(types:[TokenEnum.EXPONENT])) {
      let previousToken = self.previousToken();
      let right = self.unary();
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  // Unary expressions

  func unary() -> TreeExprType {
    if self.matchPattern(types:[TokenEnum.NOT, TokenEnum.MINUS]) {
      let previousToken = self.previousToken();
      let right = self.unary();
      return UnaryType(action: previousToken.action, right: right);
    }

    return self.literals();
  }

  // Literals

  func literals() -> TreeExprType {
    if self.matchPattern(types:[TokenEnum.TRUE]) {
        return LiteralType(value: true)
    }
    if self.matchPattern(types:[TokenEnum.FALSE]) {
        return LiteralType(value: false)
    }
    if self.matchPattern(types: [TokenEnum.NUMBER, TokenEnum.STRING]) {
      let previousToken = self.previousToken();
      return LiteralType(value: previousToken.value);
    }

    if self.matchPattern(types: methodNames) {
      let previousToken = self.previousToken();

      var nextParam = self.expression();
      var params: [Any] = [];

      while (!!nextParam) {
        params.append(nextParam);
        nextParam = self.expression();
      }

      return MethodType(action: previousToken.action, params: params);
    }

    if self.matchPattern(types: [TokenEnum.VARIABLE]) {
      let previousToken = self.previousToken();
      return VariableType(value: previousToken.action);
    }

    if self.matchPattern(types: [TokenEnum.OPEN_PAREN]) {
      let expr = self.expression();
      self.consume(type: TokenEnum.CLOSE_PAREN, message: "Expect ')' after expression.");
      return GroupingType(expr: expr);
    }
  }

  // Auxiliary expressions

  func consume(type: TokenEnumType, message: String) {
    if self.typeCheck(type: type) {
      self.currentTokenIndex+=1
    }
  }

  // Checks if the current token is one of these types, and advance
  func matchPattern(types: [TokenEnumType])-> Bool {
    for type in types {
      if self.typeCheck(type: type) {
        self.currentTokenIndex+=1
        return true;
      }
    }

    return false;
  }

  // Checks if the current token is of this type
  func typeCheck(type: TokenEnumType) -> Bool {
    if self.isEndOfExpression() {
      return false;
    }
    return self.getCurrentToken().type === type;
  }

  func previousToken() -> TokenType {
    return self.tokens[self.currentTokenIndex - 1];
  }

  func isEndOfExpression() -> Bool {
    return self.getCurrentToken().type === TokenEnum.END_OF_LINE;
  }

  func getCurrentToken() -> TokenType {
    return self.tokens[self.currentTokenIndex];
  }
}