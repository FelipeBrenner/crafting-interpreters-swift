class Token {
  var type: TokenEnum;
  var action: String;
  var codeLine: String;
  var value: String;

  init(type: TokenEnum, action: String, codeLine: String, value: String) {
    self.type = type
    self.action = action
    self.codeLine = codeLine
    self.value = value
    print("agr foi")
  }
}