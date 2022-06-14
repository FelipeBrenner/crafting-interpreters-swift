class CrawlTokens {
  var tokens: [Token]
  var currentTokenIndex: Int

  init(tokens: Token) {
    self.tokens = tokens;
    self.currentTokenIndex = 0;
  }

  func crawl() async {
    return self.expression();
  }

  func expression() {
    return self.assign();
  }

  // Binary expressions

  func assign() {
    var expr: TreeExprType = self.logical();

    while (self.matchPattern(types:[TokenEnumType.ASSIGN])) {
      let previousToken = self.previousToken();
      let right = self.logical();
      expr = AssignType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func logical() -> TreeExprType {
    var expr: TreeExprType = self.equality();

    while (self.matchPattern(types:[TokenEnumType.AND, TokenEnumType.OR, TokenEnumType.XOR])) {
      let previousToken = self.previousToken()
      let right = self.equality()
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func equality() -> TreeExprType {
    var expr: TreeExprType = self.comparation();

    while (self.matchPattern(types:[TokenEnumType.NOT_EQUAL, TokenEnumType.EQUAL])) {
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
        TokenEnumType.GREATER_THAN,
        TokenEnumType.GREATER_THAN_OR_EQUAL,
        TokenEnumType.LESS_THAN,
        TokenEnumType.LESS_THAN_OR_EQUAL]
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

    while (self.matchPattern(types:[TokenEnumType.PLUS, TokenEnumType.MINUS])) {
      let previousToken = self.previousToken();
      let right = self.multiplicationDivision();
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func multiplicationDivision() -> TreeExprType {
    var expr: TreeExprType = self.potentiation();

    while (self.matchPattern(types:[TokenEnumType.MULTIPLY, TokenEnumType.DIVIDE])) {
      let previousToken = self.previousToken();
      let right = self.potentiation();
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  func potentiation() -> TreeExprType {
    var expr: TreeExprType = self.unary();

    while (self.matchPattern(types:[TokenEnumType.EXPONENT])) {
      let previousToken = self.previousToken();
      let right = self.unary();
      expr = BinaryType(left: expr, action: previousToken.action, right: right);
    }

    return expr;
  }

  // Unary expressions

  func unary() -> TreeExprType {
    if self.matchPattern(types:[TokenEnumType.NOT, TokenEnumType.MINUS]) {
      let previousToken = self.previousToken();
      let right = self.unary();
      return UnaryType(action: previousToken.action, right: right);
    }

    return self.literals();
  }

  // Literals

  func literals() -> TreeExprType {
    if self.matchPattern(types:[TokenEnumType.TRUE]) {
        return LiteralType(value: true)
    }
    if self.matchPattern(types:[TokenEnumType.FALSE]) {
        return LiteralType(value: false)
    }
    if self.matchPattern(types: [TokenEnumType.NUMBER, TokenEnumType.STRING]) {
      let previousToken = self.previousToken();
      return LiteralType(value: previousToken.value);
    }

    if self.matchPattern(types: methodNames) {
      let previousToken = self.previousToken();

      let nextParam = self.expression();
      let params = [];

      while (!!nextParam) {
        params.push(nextParam);
        nextParam = self.expression();
      }

      return MethodType(action: previousToken.action, params: params);
    }

    if self.matchPattern(types: [TokenEnumType.VARIABLE]) {
      let previousToken = self.previousToken();
      return VariableType(action: previousToken.action);
    }

    if self.matchPattern(types: [TokenEnumType.OPEN_PAREN]) {
      let expr = self.expression();
      self.consume(TokenEnumType.CLOSE_PAREN, "Expect ')' after expression.");
      return GroupingType(expr: expr);
    }
  }

  // Auxiliary expressions

  func consume(type: TokenEnumType, message: String) throws {
    if self.typeCheck(type) {
        return self.nextToken()
    }

    throw new SyntaxError(message);
  }

  // Checks if the current token is one of these types, and advance
  func matchPattern(types: TokenEnumType...)-> Bool {
    for type in types {
      if self.typeCheck(type) {
        self.nextToken();
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
    return self.getCurrentToken()?.type === type;
  }

  func nextToken() -> TokenType {
    if !self.isEndOfExpression() {
        self.currentTokenIndex++;
    }
    return self.previousToken();
  }

  func previousToken() -> TokenType {
    return self.tokens[self.currentTokenIndex - 1];
  }

  func isEndOfExpression() {
    return self.getCurrentToken()?.type === TokenEnumType.END_OF_LINE;
  }

  func getCurrentToken() {
    return self.tokens[self.currentTokenIndex];
  }
}