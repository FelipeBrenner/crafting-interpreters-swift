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
  }

  var description: String {
    get {
        return "Token {\n\ttype: \(type.description)\n\taction: \(action)\n\tcodeLine: \(codeLine)\n\tvalue: \(value)\n}"
    }
  }
}

typealias TokenType = Token