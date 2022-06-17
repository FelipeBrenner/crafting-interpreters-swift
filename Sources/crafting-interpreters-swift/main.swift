import Foundation

let fileList = ["1-arithmetics.ehosguri", "2-relationals.ehosguri", "3-logicals.ehosguri", "4-methods.ehosguri"]

for fileName in fileList {
  let code = readFile(fileName: fileName)

  print("\n\(fileName)")

  print("\nCode:");
  print(code);
  let crawlCode = CrawlCode(code: code);
  let tokens = crawlCode.crawl();
  var trees: [TreeExprType] = [];

  // print("\nTokens:");
  // for line in tokens {
  //   for token in line {
  //     print(token.description);
  //   }
  // }

  for token in tokens {
    let crawlTokens = CrawlTokens(tokens: token);
    let tree = crawlTokens.crawl();
    if(tree != nil) {
      trees.append(tree!);
    }
  }

  // println(chalk.bgCyan.black("\nTree Object:"));
  // println(trees);

  print("\nAST Tree:");
  for tree in trees {
    // print(tree.description)
    print(TreePrinter(expr: tree).print());
  }
  // println("\nEvaluation:");
  // var evaluator = new EvaluateTree(trees);
  // evaluator.init();
  // var endExecution = new Date();
  // println(
  //     `\nend of execution ${fileList[index]} in ${
  //       endExecution.getTime() - startExecution.getTime()
  //     }ms`
  // );

  // println("------------------------------------");
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