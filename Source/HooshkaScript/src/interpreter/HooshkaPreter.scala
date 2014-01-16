package interpreter {

import parser.Parser
import generator.LLVM_generator
import scanner.ast.SingleBody
import scanner.ast.MultBody
import scanner.ast.Expression
import scanner.ast.WhileExpression

  class HooshkaPreter(parserx: Parser) {
    
    val parser = parserx;
    var ast_root = parser.get_AST_root();
    
    val dpattern = """^d[(][0-9]+[)]""".r;
    
    def interpret() {
      
    }
    
    def get_index(str: String): Int = {
      var ind = 2;
      var numstr = "";
      var s = str.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
      while (ind < s.length() - 1 || s.charAt(ind) != ')') {
        numstr = numstr + s(ind);
        ind = ind + 1;
      }
      return numstr.toInt;
    }
    
    def interactive_ast_inspect(llvm: LLVM_generator) {
      //rs -> right sibling; ls -> left sibling; v -> current node value; d(index) -> down the level (indexes from left to right); 
      //u -> parent; c -> compile; q -> quit (all interpreter commands precede with '#'); k -> show kids
      println(ast_root.toString());
      var line = Console.readLine();
      if (ast_root != null && ast_root.isInstanceOf[SingleBody]) {
        line match {
          case "#rs" => ast_root = ast_root.asInstanceOf[SingleBody].get_right;
          case "#ls" => ast_root = ast_root.asInstanceOf[SingleBody].get_left;
          case "#u" => ast_root = ast_root.asInstanceOf[Expression].get_parent;
          case "#k" => println(ast_root.asInstanceOf[SingleBody].get_left.toString() + " : " + ast_root.asInstanceOf[SingleBody].get_right.toString());
          case "#c" => llvm.compile();
          case "#q" => return;
          case _ => Console.err.println("Unkown command - single body");
        }
      }else if(ast_root != null && ast_root.isInstanceOf[MultBody]){
        line match {
          case "#u" => ast_root = ast_root.asInstanceOf[Expression].get_parent;
          case "#k" => println(ast_root.asInstanceOf[MultBody].get_body);
          case dpattern() => ast_root = ast_root.asInstanceOf[MultBody].get_body()(get_index(line));
          case "get condition" => if (ast_root.isInstanceOf[WhileExpression]) {
        	println(ast_root.asInstanceOf[WhileExpression].get_condition.toString());
          }
          case "in condition" => if (ast_root.isInstanceOf[WhileExpression]) {
            ast_root = ast_root.asInstanceOf[WhileExpression].get_condition().asInstanceOf[Expression];
          }
          case "#c" => llvm.compile();
          case "#q" => return;
          case _ => Console.err.println("UNkown command - multiple body");
        }
      }else{
    	  
      }
      var first_ch = line.charAt(0);
      if (first_ch == '#') {
        interactive_ast_inspect(llvm);
      }else{
        println(line);
        interactive_ast_inspect(llvm);
      }
    }
  }
}