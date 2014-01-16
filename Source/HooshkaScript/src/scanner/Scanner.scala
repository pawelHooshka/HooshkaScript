package scanner {

import java.io.BufferedReader
import java.io.FileReader
import java.io.IOException
import scanner.ast.Expression
import scanner.ast.OpenRoundBracket
import scanner.ast.ClosedRoundBracket
import scanner.ast.OpenCurlyBracket
import scanner.ast.ClosedCurlyBracket
import scanner.ast.OpenSquareBracket
import scanner.ast.ClosedSquareBracket
import scanner.ast.LoopExpression
import scanner.ast.LoopForExpression
import scanner.ast.VariableExpression
import scanner.ast.IteratorCallingExpression
import scanner.ast.ProcedureCallingExpression
import scanner.ast.IteratorDefinitionExpression
import scanner.ast.ProcedureDefinitionExpression
import java.util.ArrayList
import scanner.ast.IfExpression
import scanner.ast.ElifExpression
import scanner.ast.ElseExpression
import scanner.ast.NumberExpression
import scanner.ast.CharacterExpression
import scanner.ast.StringExpression
import scanner.ast.ClassExpression
import scanner.ast.ExtendsExpression
import scanner.ast.BreakExpression
import scanner.ast.RepeatExpression
import scanner.ast.ReturnExpression
import scanner.ast.BinaryExpression
import scanner.ast.RelationalExpression
import scanner.ast.ConditionOperatorExpression
import scanner.ast.Brackets
import scanner.ast.WhileExpression

class Scanner(start_file: String, files: List[String]) {
  
  var counter = 0;
  
  var buffered_reader: BufferedReader = null;
  var file_reader: FileReader = null;

  var sources = files;
  var assistant = new ScannerAssistant();
  
  var last_string_token_id: String = null;
  var last_token: Expression = null;
  var last_token_index = 0;
  var in_loop_for_token = false;
  
  val iter_or_proc_CallPattern = """([a-zA-Z_0-9][(])""".r;
  val variablePattern = """[a-z]{1}[a-zA-Z0-9_]*""".r;
  val variablePatternTest = """([a-z]){1}""".r;
  val intPattern = """[0-9]+""".r;
  val realPattern = """[0-9]+\.{1}[0-9]+""".r;
  val charPattern = """^\'.{1}\'$""".r;
  val stringPattern = """^\".*\"$""".r;
  val binaryPattern = """[\+\-\=\*\/\^\%]{1}""".r;
  val other_binaryPattern = List[String](":-", "><");
  val class_name_pattern = """[A-Z]+.*""".r;
  
  def init_reader(): BufferedReader = {
    file_reader = new FileReader(start_file);
    return new BufferedReader(file_reader);
  }
  
  def start_scann() {
    buffered_reader = init_reader();
  }
  
  def start_scann(file: String): BufferedReader = {
    file_reader = new FileReader(file);
    return new BufferedReader(file_reader);
  }
  
  def nextLine(): List[Expression] = {
    var line_of_tokens = List[Expression]();
    counter = counter + 1;
    var token_in_line_counter = 0;
    var in_token_counter = 0;
    //var bracket: Brackets = null;
    var line_of_pre_tokens: List[String] = assistant.process_line(buffered_reader.readLine(), counter, variablePattern);
    if (line_of_pre_tokens == null) {
      return null;
    }
    var len = line_of_pre_tokens.length - 1;
    for (token_str <- line_of_pre_tokens) {
      //println(assistant.is_type(token_str) + " : " + token_str);
      var node: Expression = null;
      token_str match {
        case "while" => node = new WhileExpression(null, null);
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "while loop";
        case "loop" => node = new LoopExpression(null);
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "loop";
        case "loop for" => node = new LoopForExpression(null, null, null);
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "loop for";
          in_loop_for_token = true;
        case "over" =>
          //skip to iterator. We already know that there must be 'over' in 'loop for' construct.
          last_string_token_id = "loop for _ over";
        case iter_or_proc_CallPattern() =>
          if (in_loop_for_token) {
            node = new IteratorCallingExpression(null);
            println("LAST NODE: " + node.get_type());
            in_loop_for_token = false;
          }else{
            node = new ProcedureCallingExpression(null, null);
            println("LAST NODE: " + node.get_type());
          }
          line_of_tokens ::= node;
        case "if" => node = new IfExpression(null, null);
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "in if token";
        case "elif" => node = new ElifExpression(null, null, null);
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "in elif";
        case "else" => node = new ElseExpression(null);
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "in else";
        case "iter" => node = new IteratorDefinitionExpression(null, null);
          println("LAST NODE: " + node.get_type());
          last_string_token_id = "iter declaration";
          line_of_tokens ::= node;
        case "proc" => node = new ProcedureDefinitionExpression(null, null);
          println("LAST NODE: " + node.get_type());
          last_string_token_id = "proc declaration";
          line_of_tokens ::= node;
        case "class" =>
          var name: String = null;
          if (line_of_pre_tokens.length <= 1) {
            Console.err.println("Error: line to short for class definition. ");
          }else{
            name = line_of_pre_tokens(1);
          }
          var node = new ClassExpression(name);
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "class";
        case "break" => node = new BreakExpression();
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "break";
        case "repeat" => node = new RepeatExpression();
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "repeat";
        case "return" => node = new ReturnExpression();
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "return";
        case variablePattern() => 
          if (token_in_line_counter < len && line_of_pre_tokens(token_in_line_counter + 1) == "(" && in_loop_for_token) {
            var node = new IteratorCallingExpression(null);
            in_loop_for_token = false;
            line_of_tokens ::= node;
            println("LAST NODE: " + node.get_type());
            last_token_index = token_in_line_counter;
            last_string_token_id = "iterator_call";
            last_token = node;
          }else if (token_in_line_counter < len && line_of_pre_tokens(token_in_line_counter + 1) == "(" && !in_loop_for_token) {
            var node = new ProcedureCallingExpression(null, null);
            line_of_tokens ::= node;
            println("LAST NODE: " + node.get_type());
            last_token_index = token_in_line_counter;
            last_string_token_id = "procedure_call";
            last_token = node;
          }else{
            var node2: VariableExpression = new VariableExpression(token_str, -1);
            line_of_tokens ::= node2;
            last_token = node2;
            println("LAST NODE: " + node2.get_type() + " : " + node2.get_variable_name());
            last_string_token_id = token_str;
            last_token_index = token_in_line_counter;
          }
        case realPattern() => var node3 = new NumberExpression(null, "real", 0, 0);
          var dot_index = token_str.indexOf(".");
          var int_str = token_str.substring(0, dot_index);
          var dec_str = token_str.substring(dot_index + 1, token_str.length());
          node3.set_integer_part(int_str.toInt);
          node3.set_decimal_part(dec_str.toInt);
          //var node3 = new NumberExpression(null, "real", int_str.toInt, dec_str.toInt);
          println("LAST NODE: " + node3.get_type() + " : " + node3.get_number_type() + " : " + node3.get_integer_part() + "." + node3.get_decimal_part());
          line_of_tokens ::= node3;
          last_token = node3;
          last_string_token_id = "real";
        case intPattern() => var node_num = new NumberExpression(null, "int", token_str.toInt, 0);
          println("LAST NODE: " + node_num.get_type() + " : " + node_num.get_number_type() + " : " + node_num.get_integer_part());
          line_of_tokens ::= node_num;
          last_token = node_num;
          last_string_token_id = "int";
        case charPattern() => var node = new CharacterExpression(token_str.substring(1, token_str.length() - 1));
          println("LAST NODE: " + node.get_type() + " : " + node.get_value());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "character";
        case stringPattern() => var node = new StringExpression(token_str.substring(1, token_str.length() - 1));
          println("LAST NODE: " + node.get_type() + " : " + node.get_value());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "string";
        case "extends" => var node = new ExtendsExpression(null, null);
          println("LAST NODE: " + node.get_type());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "extends";
        case binaryPattern() => var node_bin: BinaryExpression = new BinaryExpression(token_str, null, null);
          println("LAST NODE: " + node_bin.get_type() +  " : " + node_bin.get_bin_op());
          line_of_tokens ::= node_bin;
          last_token = node_bin;
          last_string_token_id = "binary op";
        case  ":-" | "><" => var node_bin: BinaryExpression = new BinaryExpression(token_str, null, null);
          println("LAST NODE: " + node_bin.get_type() + " : " + node_bin.get_bin_op());
          line_of_tokens ::= node_bin;
          last_token = node_bin;
          last_string_token_id = "binary op";
          //relational operators:
        case ">" | "<" | "==" | "<=" | ">=" => var node_rel: RelationalExpression = new RelationalExpression(token_str, null, null);
          println("LAST NODE: " + node_rel.get_type() + " : " + node_rel.get_operator());
          line_of_tokens ::= node_rel;
          last_token = node_rel;
          last_string_token_id = "relational op";
        case "||" | "&&" | "|" | "~" => var node_cond: ConditionOperatorExpression = new ConditionOperatorExpression(token_str, null, null);
          println("LAST NODE: " + node_cond.get_type() + " : " + node_cond.get_operator());
          line_of_tokens ::= node;
          last_token = node;
          last_string_token_id = "condition logic op";
        case "(" => var bracket = new OpenRoundBracket();
          line_of_tokens ::= bracket;
          println("Node is: " + bracket.get_type() + " : " + bracket.get_bracket());
        case ")" => var bracket = new ClosedRoundBracket();
          line_of_tokens ::= bracket;
          println("Node is: " + bracket.get_type() + " : " + bracket.get_bracket());
        case "{" => var bracket = new OpenCurlyBracket();
          line_of_tokens ::= bracket;
          println("Node is: " + bracket.get_type() + " : " + bracket.get_bracket());
        case "}" => var bracket = new ClosedCurlyBracket();
          line_of_tokens ::= bracket;
          println("Node is: " + bracket.get_type() + " : " + bracket.get_bracket());
        case "[" => var bracket = new OpenSquareBracket();
          line_of_tokens ::= bracket;
          println("Node is: " + bracket.get_type() + " : " + bracket.get_bracket());
        case "]" => var bracket = new ClosedSquareBracket();
          line_of_tokens ::= bracket;
          println("Node is: " + bracket.get_type() + " : " + bracket.get_bracket());
        case _ =>
          println("No Match. ");
      }
     // if (last_token.isInstanceOf[VariableExpression] && line_of_tokens(0).isInstanceOf[OpenRoundBracket]) {
      //  var node: Expression = null;
      //  if (in_loop_for_token) {
     //     node = new IteratorCallingExpression(null);
     //     in_loop_for_token = false;
     //   }else{
     //     node = new ProcedureCallingExpression(null, null);
     //   }
      //  line_of_tokens = line_of_tokens.patch(2, Seq(node), 1);
      //}
      token_in_line_counter = token_in_line_counter + 1;
    }
    if (line_of_pre_tokens == null) {
      return null;
    }else{
      return line_of_tokens.reverse;
    }
  }
  
  def close(): Boolean = {
    if (buffered_reader != null) {
      try{
        buffered_reader.close();
        return true;
      }catch{
        case ex: IOException =>
          ex.printStackTrace();
          return false;
      }
    }else{
      return false;
    }
  }
}
}