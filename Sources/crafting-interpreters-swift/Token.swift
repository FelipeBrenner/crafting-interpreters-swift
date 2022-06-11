class Token {
  var type: TokensEnum.TokenEnum;
  var action: String;
  var codeLine: String;
  var value: String;

  init(type: TokensEnum.TokenEnum, action: String, codeLine: String, value: String) {
    self.type = type
    self.action = action
    self.codeLine = codeLine
    self.value = value
    print("agr foi")
  }
}
typealias TokenType = Token