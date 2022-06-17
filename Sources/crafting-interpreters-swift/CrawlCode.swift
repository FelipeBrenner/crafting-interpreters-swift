import Foundation
class CrawlCode {
  var tokens: [[TokenType]];
  var currentCharIndex: Int;
  var startCharIndex: Int;
  var line: Int;
  var code: String;

  init(code: String) {
    self.tokens = [[]];
    self.currentCharIndex = 0;
    self.startCharIndex = 0;
    self.line = 1;
    self.code = code;
  }

  func crawl() -> [[TokenType]] {
    while (!self.isEndOfExpression()) {
      self.startCharIndex = self.currentCharIndex;
      self.scanForTokens();
    }

    return self.tokens;
  }

  func scanForTokens() -> Void {
    let currentChar = String(self.code[self.code.index(self.code.startIndex, offsetBy: self.currentCharIndex)])
    self.nextCharacter();
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
        self.tokens.append([])
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
    
    if (self.getCharAtCurrent() == TokenEnum.FLOAT_DELIMITER.value) {
      self.nextCharacter();
      while (self.isDigit(digit: self.getCharAtCurrent())) {
        self.nextCharacter();
      }
    }

    let value = Double(substring(str: self.code, start: self.startCharIndex, final: self.currentCharIndex))!;
    self.addToken(tokenEnum: TokenEnum.NUMBER, value: value);
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

    let value = substring(str: self.code, start: self.startCharIndex + 1, final: self.currentCharIndex - 1);
    self.addToken(tokenEnum: TokenEnum.STRING, value: value);
  }

  func handleReservedWords() -> Void {
    while (self.isAlpha(alpha: self.getCharAtCurrent())) {
      self.nextCharacter();
    }

    let text = substring(str: self.code, start: self.startCharIndex, final: self.currentCharIndex);
    var type = reservedWords.first(where: {$0.value == text});
    if (type == nil) {
      type = TokenEnum.VARIABLE;
    } 
    self.addToken(tokenEnum: type!);
  }

  func getCharAtCurrent() -> String {
    if(self.isEndOfExpression()) {
      return ""
    }
    
    let index = self.code.index(self.code.startIndex, offsetBy: self.currentCharIndex)
    return String(self.code[index]);
  }

  func matchRegex(expression: String) -> Bool {
    let range = self.getCharAtCurrent().range(of: expression, options: .regularExpression);
    return range != nil;
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

  func addToken(tokenEnum: TokenEnumType, value: Any = "") -> Void {
    let text = substring(str: self.code, start: self.startCharIndex, final: self.currentCharIndex);
    let lineIndex = self.line - 1;
    self.tokens[lineIndex].append(Token(type: tokenEnum, action: text, codeLine: self.code, value: value));
  }

  func isEndOfExpression() -> Bool {
    return self.currentCharIndex >= self.code.count;
  }
}

func substring(str: String, start: Int, final: Int) -> String {
  let start = str.index(str.startIndex, offsetBy: start)
  let end = str.index(str.endIndex, offsetBy: -(str.count-final))
  let range = start..<end
  return "\(str[range])"
}