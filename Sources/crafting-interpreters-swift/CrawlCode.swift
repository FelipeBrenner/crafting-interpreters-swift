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

  func crawl() async {
    while (!self.isEndOfExpression()) {
      self.startCharIndex = self.currentCharIndex;
      self.scanForTokens();
    }

    return self.tokens;
  }

  func scanForTokens() {
    let currentChar = self.nextCharacter();
    switch currentChar {
      case TokenEnum.OPEN_PAREN.value:
        self.addToken(TokenEnum.OPEN_PAREN);
        break;
      case TokenEnum.CLOSE_PAREN.value:
        self.addToken(TokenEnum.CLOSE_PAREN);
        break;
      case TokenEnum.MINUS.value:
        self.addToken(TokenEnum.MINUS);
        break;
      case TokenEnum.PLUS.value:
        self.addToken(TokenEnum.PLUS);
        break;
      case TokenEnum.MULTIPLY.value:
        self.addToken(TokenEnum.MULTIPLY);
        break;
      case TokenEnum.DIVIDE.value:
        self.addToken(TokenEnum.DIVIDE);
        break;
      case TokenEnum.EXPONENT.value:
        self.addToken(TokenEnum.EXPONENT);
        break;
      case TokenEnum.ASSIGN.value:
        self.addToken(
          self.matchNext(TokenEnum.EQUAL.expected)
            ? TokenEnum.EQUAL
            : TokenEnum.ASSIGN
        );
        break;
      case TokenEnum.NOT.value:
        self.addToken(
          self.matchNext(TokenEnum.NOT_EQUAL.expected)
            ? TokenEnum.NOT_EQUAL
            : TokenEnum.NOT
        );
        break;
      case TokenEnum.LESS_THAN.value:
        self.addToken(
          self.matchNext(TokenEnum.LESS_THAN_OR_EQUAL.expected)
            ? TokenEnum.LESS_THAN_OR_EQUAL
            : TokenEnum.LESS_THAN
        );
        break;
      case TokenEnum.GREATER_THAN.value:
        self.addToken(
          self.matchNext(TokenEnum.GREATER_THAN_OR_EQUAL.expected)
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
        self.line++;
        break;
      default:
        if (self.isDigit(currentChar)) {
          self.handleNumber();
        } else if (self.isAlpha(currentChar)) {
          self.handleReservedWords();
        } else {
          console.error(currentChar + " - Unknown token");
        }
        break;
    }

    return self.tokens;
  }

  func isDigit(digit: String) {
    return digit >= "0" && digit <= "9";
  }

  func isAlpha(alpha: String) {
    return (
      (alpha >= "a" && alpha <= "z") ||
      (alpha >= "A" && alpha <= "Z") ||
      alpha === "_"
    );
  }

  func handleNumber() {
    while (self.isDigit(self.getCharAtCurrent())) {
      self.nextCharacter();
    }

    if (
      self.getCharAtCurrent() == TokenEnum.FLOAT_DELIMITER.value &&
      self.isDigit(self.matchNext("[0-9]"))
    ) {
      self.nextCharacter();
      while (self.isDigit(self.getCharAtCurrent())) {
        self.nextCharacter();
      }
    }

    let value = self.code.substring(
      self.startCharIndex,
      self.currentCharIndex
    );

    self.addToken(TokenEnum.NUMBER, parseFloat(value));
  }

  func handleString() {
    while (
      self.getCharAtCurrent() != TokenEnum.STRING_DELIMITER.value &&
      !self.isEndOfExpression()
    ) {
      if (self.getCharAtCurrent() == TokenEnum.END_OF_LINE.value) {
        self.line++;
      }
      self.currentCharIndex++;
    }

    if (self.isEndOfExpression()) {
      console.error("String not finished till end of code");
      return;
    }

    self.currentCharIndex++;

    let value = self.code.substring(
      self.startCharIndex + 1,
      self.currentCharIndex - 1
    );
    self.addToken(TokenEnum.STRING, value);
  }

  func handleReservedWords() {
    while (self.isAlpha(self.getCharAtCurrent())) {
      self.nextCharacter();
    }

    let text = self.code.slice(self.startCharIndex, self.currentCharIndex);
    let type = reservedWords.find((word) => word.value === text);
    if (!type) {
      type = TokenEnum.VARIABLE;
    } 
    self.addToken(type);
  }

  func getCharAtCurrent() {
    return self.code[self.currentCharIndex];
  }

  func matchRegex(expression: String) {
    let regExp = new RegExp(expression);
    return regExp.exec(self.getCharAtCurrent()) != null;
  }

  func matchNext(expected: String) {
    if (self.isEndOfExpression() || !self.matchRegex(expected)) {
      return false;
    }

    self.currentCharIndex++;
    return true;
  }

  func nextCharacter() {
    return self.code.charAt(self.currentCharIndex++);
  }

  func addToken(tokenEnum: String, value: String) {
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

  func isEndOfExpression() {
    return self.currentCharIndex >= self.code.count;
  }
}
