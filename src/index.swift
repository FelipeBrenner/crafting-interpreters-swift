import { CrawlCode } from "./CrawlCode.js";
import { CrawlTokens } from "./CrawlTokens.js";
import { EvaluateTree } from "./EvaluateTree.js";
import { TreePrinter } from "./TreePrinter.js";

async func main() {
  // let fileList = fs.readdirSync("scripts");

  // for (var index in fileList) {
    var startExecution = new Date();
    println(`running ${fileList[index]}`);
    // var code = fs.readFileSync("scripts/" + fileList[index], {
    //   encoding: "utf8",
    // });
    var code = "2 + 2"

    println("\nCode:");
    println(code);
    var crawlCode = new CrawlCode(code);
    var tokens = await crawlCode.crawl();

    var trees = [];

    // println(chalk.bgCyan.black("\nTokens:"));
    // println(tokens);

    for (var token of tokens) {
      var crawlTokens = new CrawlTokens(token);
      var tree = await crawlTokens.crawl();
      trees.push(tree);
    }

    // println(chalk.bgCyan.black("\nTree Object:"));
    // println(trees);

    println("\nAST Tree:");
    for (var tree of trees) {
      println(new TreePrinter(tree).print());
    }
    println("\nEvaluation:");
    var evaluator = new EvaluateTree(trees);
    evaluator.init();
    var endExecution = new Date();
    println(
        `\nend of execution ${fileList[index]} in ${
          endExecution.getTime() - startExecution.getTime()
        }ms`
    );

    println("------------------------------------");
  // }
}

main();
