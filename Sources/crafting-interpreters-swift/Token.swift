class Token {
  var type: TokenEnumType;
  var action: String;
  var codeLine: String;
  var value: Any;

  init(type: TokenEnumType, action: String, codeLine: String, value: Any) {
    self.type = type
    self.action = action
    self.codeLine = codeLine
    self.value = value
    print("agr foi")
  }
}

typealias TokenType = Token