class TokensEnum {
  // Literals
  let STRING = TokenEnum(value: "STRING");
  let NUMBER = TokenEnum(value: "NUMBER");
  let STRING_DELIMITER = TokenEnum(value: "'");
  let FLOAT_DELIMITER = TokenEnum(value: ".");
  let FALSE = TokenEnum(value: "false");
  let TRUE = TokenEnum(value: "true");

  // Parentheses
  let OPEN_PAREN = TokenEnum(value: "(");
  let CLOSE_PAREN = TokenEnum(value: ")");

  // Unary expressions
  let NOT = TokenEnum(value: "!");
  let MINUS = TokenEnum(value: "-");

  // Binary expressions
  let EXPONENT = TokenEnum(value: "^");
  let MULTIPLY = TokenEnum(value: "*");
  let DIVIDE = TokenEnum(value: "/");
  let PLUS = TokenEnum(value: "+");
  let ASSIGN = TokenEnum(value: "=");
  let LESS_THAN = TokenEnum(value: "<");
  let GREATER_THAN = TokenEnum(value: ">");
  let EQUAL = TokenEnum(value: "=", expected: "=");
  let NOT_EQUAL = TokenEnum(value: "!", expected: "=");
  let GREATER_THAN_OR_EQUAL = TokenEnum(value: ">", expected: "=");
  let LESS_THAN_OR_EQUAL = TokenEnum(value: "<", expected: "=");
  let AND = TokenEnum(value: "tambem");
  let OR = TokenEnum(value: "alterna");
  let XOR = TokenEnum(value: "xorabb");

  // Reserved words - methods
  let OUTPUT = TokenEnum(value: "chora");
  let MAX = TokenEnum(value: "chamaomaior");
  let MIN = TokenEnum(value: "chamaomenor");
  let AVG = TokenEnum(value: "mediazinhacpx");
  let SUM = TokenEnum(value: "somaaaa");

  let VARIABLE = TokenEnum(value: "VARIABLE");

  let END_OF_LINE = TokenEnum(value: "\n");

  class TokenEnum {
    var value: String;
    var expected: String;
    var final: String;

    init (value: String, expected: String = "") {
      self.value = value;
      self.expected = expected;
      self.final = value + expected;
    }

    var description: String {
      get {
          return "TokenEnum {\n\t\tvalue: \(value)\n\t\texpected: \(expected)\n\t\tfinal: \(final)\n\t}"
      }
    }
  }
}

let TokenEnum = TokensEnum();
typealias TokenEnumType = TokensEnum.TokenEnum
