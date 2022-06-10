class CrawlCode {
  var tokens: [Token];
  var currentCharIndex: Int;
  var startCharIndex: Int;
  var line: Int;
  var code: String;

  init(code: String) {
    self.tokens = [];
    self.currentCharIndex = 0;
    self.startCharIndex = 0;
    self.line = 1;
    self.code = code;
  }

  func crawl() async -> [Token] {
    while (!self.isEndOfExpression()) {
      self.startCharIndex = self.currentCharIndex;
      self.scanForTokens();
    }

    return self.tokens;
  }

  func scanForTokens() -> Void {
    self.nextCharacter();
    let currentChar = String(self.code[self.code.index(self.code.startIndex, offsetBy: self.currentCharIndex)])
    switch currentChar {
      case TokenEnum.OPEN_PAREN.value:
        self.addToken(tokenEnum: TokenEnum.OPEN_PAREN);
        break;
      case TokenEnum.CLOSE_PAREN.value:
        self.addToken(tokenEnum: TokenEnum.CLOSE_PAREN);
        break;
      case TokenEnum.MINUS.value:
        self.addToken(tokenEnum: TokenEnum.MINUS);
        break;
      case TokenEnum.PLUS.value:
        self.addToken(tokenEnum: TokenEnum.PLUS);
        break;
      case TokenEnum.MULTIPLY.value:
        self.addToken(tokenEnum: TokenEnum.MULTIPLY);
        break;
      case TokenEnum.DIVIDE.value:
        self.addToken(tokenEnum: TokenEnum.DIVIDE);
        break;
      case TokenEnum.EXPONENT.value:
        self.addToken(tokenEnum: TokenEnum.EXPONENT);
        break;
      case TokenEnum.ASSIGN.value:
        self.addToken(tokenEnum: 
          self.matchNext(expected: TokenEnum.EQUAL.expected)
            ? TokenEnum.EQUAL
            : TokenEnum.ASSIGN
        );
        break;
      case TokenEnum.NOT.value:
        self.addToken(tokenEnum: 
          self.matchNext(expected: TokenEnum.NOT_EQUAL.expected)
            ? TokenEnum.NOT_EQUAL
            : TokenEnum.NOT
        );
        break;
      case TokenEnum.LESS_THAN.value:
        self.addToken(tokenEnum: 
          self.matchNext(expected: TokenEnum.LESS_THAN_OR_EQUAL.expected)
            ? TokenEnum.LESS_THAN_OR_EQUAL
            : TokenEnum.LESS_THAN
        );
        break;
      case TokenEnum.GREATER_THAN.value:
        self.addToken(tokenEnum: 
          self.matchNext(expected: TokenEnum.GREATER_THAN_OR_EQUAL.expected)
            ? TokenEnum.GREATER_THAN_OR_EQUAL
            : TokenEnum.GREATER_THAN
        );
        break;
      case TokenEnum.STRING_DELIMITER.value:
        self.handleString();
        break;
      case " ", "\r", "\t":
        break;
      case "\n":
        self.line+=1;
        break;
      default:
        if (self.isDigit(digit: currentChar)) {
          self.handleNumber();
        } else if (self.isAlpha(alpha: currentChar)) {
          self.handleReservedWords();
        } else {
          print(currentChar + " - Unknown token");
        }
        break;
    }
  }

  func isDigit(digit: String) -> Bool {
    return digit >= "0" && digit <= "9";
  }

  func isAlpha(alpha: String) -> Bool {
    return (
      (alpha >= "a" && alpha <= "z") ||
      (alpha >= "A" && alpha <= "Z") ||
      alpha == "_"
    );
  }

  func handleNumber() -> Void {
    while (self.isDigit(digit: self.getCharAtCurrent())) {
      self.nextCharacter();
    }

    if (
      self.getCharAtCurrent() == TokenEnum.FLOAT_DELIMITER.value &&
      self.isDigit(digit: self.matchNext(expected: "[0-9]"))
    ) {
      self.nextCharacter();
      while (self.isDigit(digit: self.getCharAtCurrent())) {
        self.nextCharacter();
      }
    }

    let value = self.code.substring(
      self.startCharIndex,
      self.currentCharIndex
    );

    self.addToken(tokenEnum: TokenEnum.NUMBER, parseFloat(value));
  }

  func handleString() -> Void {
    while (
      self.getCharAtCurrent() != TokenEnum.STRING_DELIMITER.value &&
      !self.isEndOfExpression()
    ) {
      if (self.getCharAtCurrent() == TokenEnum.END_OF_LINE.value) {
        self.line+=1;
      }
      self.currentCharIndex+=1;
    }

    if (self.isEndOfExpression()) {
      print("String not finished till end of code");
      return;
    }

    self.currentCharIndex+=1;

    let value = self.code.substring(
      self.startCharIndex + 1,
      self.currentCharIndex - 1
    );
    self.addToken(tokenEnum: TokenEnum.STRING, value);
  }

  func handleReservedWords() -> Void {
    while (self.isAlpha(alpha: self.getCharAtCurrent())) {
      self.nextCharacter();
    }

    let text = self.code.slice(self.startCharIndex, self.currentCharIndex);
    let type = reservedWords.find((word) => word.value == text);
    if (!type) {
      type = TokenEnum.VARIABLE;
    } 
    self.addToken(tokenEnum: type);
  }

  func getCharAtCurrent() -> String {
    return String(self.code[self.currentCharIndex]);
  }

  func matchRegex(expression: String) -> Bool {
    let regExp = new RegExp(expression);
    return regExp.exec(self.getCharAtCurrent()) != nil;
  }

  func matchNext(expected: String) -> Bool {
    if (self.isEndOfExpression() || !self.matchRegex(expression: expected)) {
      return false;
    }

    self.currentCharIndex+=1;
    return true;
  }

  func nextCharacter() -> Void {
    self.currentCharIndex+=1;
  }

  func addToken(tokenEnum: TokenEnum, value: String = "") -> Void {
    let text = self.code.substring(
      self.startCharIndex,
      self.currentCharIndex
    );
    let lineIndex = self.line - 1;
    if (!self.tokens[lineIndex]) {
      self.tokens[lineIndex] = [];
    }
    self.tokens[lineIndex].push(Token(type: tokenEnum, action: text, codeLine: self.line, value: value));
  }

  func isEndOfExpression() -> Bool {
    return self.currentCharIndex >= self.code.count;
  }
}