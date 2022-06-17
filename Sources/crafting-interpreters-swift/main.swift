// let fileList = fs.readdirSync("scripts");

// for (var index in fileList) {
  // var startExecution = new Date();
  // println(`running ${fileList[index]}`);
  // var code = fs.readFileSync("scripts/" + fileList[index], {
  //   encoding: "utf8",
  // });
  let code = "a = 2 * 2 + 2^3"

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
// }