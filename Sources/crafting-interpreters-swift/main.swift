import Foundation

let fileList = ["1-arithmetics.ehosguri", "2-relationals.ehosguri", "3-logicals.ehosguri", "4-methods.ehosguri", "5-ternary-conditional.ehosguri"]

for fileName in fileList {
  let code = readFile(fileName: fileName)

  print("\n\(fileName)")

  print("\nCode:");
  print(code);
  let crawlCode = CrawlCode(code: code);
  let tokens = crawlCode.crawl();
  var trees: [TreeExprType] = [];

  for token in tokens {
    let crawlTokens = CrawlTokens(tokens: token);
    let tree = crawlTokens.crawl();
    if(tree != nil) {
      trees.append(tree!);
    }
  }

  print("\nAST Tree:");
  for tree in trees {
    print(TreePrinter(expr: tree).print());
  }

  print("\nEvaluation:");
  let evaluator = EvaluateTree(trees: trees);
  evaluator.start();
}

func readFile(fileName: String) -> String {
  let file = FileManager.default.currentDirectoryPath + "/scripts/" + fileName;
  var lines: String

  do {
    lines = try String(contentsOfFile: file)
  } catch {
    return (error.localizedDescription)
  }

  return lines
}