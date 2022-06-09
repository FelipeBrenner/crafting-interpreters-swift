class Token {
  var type: String;
  var action: String;
  var codeLine: String;
  var value: String;

  init(type: String, action: String, codeLine: String, value: String) {
    self.type = type
    self.action = action
    self.codeLine = codeLine
    self.value = value
    print("agr foi")
  }
}