package parser{

import scanner.ast
import scanner.Scanner
import scanner.ast.Expression
import scanner.ast.GlobalExpression
import scala.collection.mutable.Queue
import scanner.ast.Brackets
import scanner.ast.BinaryExpression
import scanner.ast.IfExpression
import scanner.ast.Definition
import scanner.ast.MultBody
import scanner.ast.NumberExpression
import scanner.ast.VariableExpression
import scanner.ast.ProcedureCallingExpression
import scanner.ast.IteratorCallingExpression
import scanner.ast.Booleanable
import scanner.ast.ConditionOperatorExpression
import scanner.ast.RelationalExpression
import scanner.ast.Opennable
import scanner.ast.FlowControlExpression
import scanner.ast.ElifExpression
import scanner.ast.LoopForExpression
import scala.collection.mutable.Stack
import scanner.ast.SingleBody
import scanner.ast.ClassExpression
import scanner.ast.ReturnExpression
import scanner.ast.Signable
import scanner.ast.ElseExpression
import scanner.ast.ClosedCurlyBracket
import scanner.ast.OpenCurlyBracket
import scanner.ast.WhileExpression

	class Parser(scannerx: Scanner) {
  
		var global_scope: GlobalExpression = new GlobalExpression();
  
		var rootAST: Expression = global_scope;
  
		var current_first_of_type_index = -1;
		var current_param_list = List[Expression]();

		val scanner = scannerx;
		
		var last_scope: Expression = global_scope;
		var current_scope: Expression = global_scope;
		var next_possible_scope: Expression = rootAST;
		var previous_scopes_queue = Stack[Expression]();
		
		val meanings = Map("arithmetic_or_concat" -> 1, "boolean" -> 2, "syntax_error" -> 3);
		
		var now_new_scope: Expression = null;
		
		var close_scope = false;
		
		previous_scopes_queue.push(next_possible_scope);
		
		def is_scope_empty(): Boolean = {
		  if (previous_scopes_queue.isEmpty) {
		    return true;
		  }
		  return false;
		}
		
		def enqueue_scope(elem: Expression) {
		  previous_scopes_queue.push(elem);
		  println("New scope has been just added onto the stack.: " + elem.get_type);
		  println(previous_scopes_queue);
		  println();
		}
		
		def head_scope(): Expression = {
		  return previous_scopes_queue.head;
		} 
		
		def dequeue_scope(): Expression = {
		  try {
		    var r = previous_scopes_queue.pop();
		    println("Scope node has just been removed from the stack.: " + r.get_type);
		    println(previous_scopes_queue);
		    println();
		    return r;
		  }catch{
		    case ex: NoSuchElementException => return null;
		  }
		}
		
		def test_run(scanner: Scanner) {
			scanner.start_scann();
			//var value_list = scanner.nextLine();
			var value_list: List[Expression] = null;
			do {
			    println();
				value_list = scanner.nextLine();
				if (value_list != null) {
					for (token <- value_list) {
						println(token.get_type());
					}  
				}
				println();
			}while(value_list != null);
			scanner.close;
		}
		
		def is_of_type(node: Expression, typ: String): Boolean = {
		  if (node.get_type() == typ) {
		    return true;
		  }
		  return false;
		}
		
		def get_first_of_type(t: String, nodes: List[Expression]): Expression = {
		  var index = 0;
		  for (n <- nodes) {
		    if (n.get_type() == t) {
		      current_first_of_type_index = index;
		      return n;
		    }
		    index = index + 1;
		  }
		  return null;
		}
		
		def get_index_of_first_closing_round_br(nodes: List[Expression]): Int = {
		  var index = 0;
		  for (n <- nodes) {
		    if (is_closing_round_br(n)) {
		      return index;
		    }
		    index = index + 1;
		  }
		  return -1;
		}
		
		
		def get_last_index_of_curly_close(nodes: List[Expression]): Int = {
		  var index = 0;
		  var answ = -1;
		  for (n <- nodes) {
		    if (n.get_type == "bracket" && n.asInstanceOf[Brackets].get_bracket == "}") {
		      answ = index;
		    }
		    index = index + 1;
		  }
		  return answ;
		}
		
		
		def get_first_index_of_curly_open(nodes: List[Expression]): Int = {
		  var index = 0;
		  for (n <- nodes) {
		    if (n.get_type == "bracket" && n.asInstanceOf[Brackets].get_bracket == "{") {
		      return index;
		    }
		    index = index + 1;
		  }
		  return -1;
		}
		
		
		def get_last_index_of_closing_round_br(nodes: List[Expression]): Int = {
		  var index = 0;
		  var answer = -1;
		  for (n <- nodes) {
		    if (is_closing_round_br(n)) {
		      answer = index;
		    }
		    index = index + 1;
		  }
		  return answer;
		}
		
		def get_first_index_of_open_round_br(nodes: List[Expression]): Int = {
		  var index = 0;
		  for (n <- nodes) {
		    if (is_open_round_br(n)) {
		      return index;
		    }
		    index = index + 1;
		  }
		  return -1;
		}
		
		def get_last_index_of_closing_round_br_until_curly_open(nodes: List[Expression]): Int = {
		  var index = 0;
		  var answer = -1;
		  for (n <- nodes) {
		    if (is_closing_round_br(n)) {
		      answer = index;
		    }else if (is_curly_openning(n)) {
		      return answer;
		    }
		    index = index + 1;
		  }
		  return answer;
		}
		
		def is_closing_round_br(n: Expression): Boolean = {
		  if (n.get_type == "bracket") {
		    if (n.asInstanceOf[Brackets].get_bracket == ")") {
		      return true;
		    }
		  }
		  return false;
		}
		
		def is_open_round_br(n: Expression): Boolean = {
		  if (n.get_type == "bracket") {
		    if (n.asInstanceOf[Brackets].get_bracket == "(") {
		      return true;
		    }
		  }
		  return false;
		}
		
		def is_curly_openning(n: Expression): Boolean = {
		  if (n.get_type == "bracket") {
		    if (n.asInstanceOf[Brackets].get_bracket == "{") {
		      return true;
		    }
		  }
		  return false;
		}
		
		def get_sub_list(nodes: List[Expression], start: Int, stop: Int): List[Expression] = {
		  var l = List[Expression]();
		  var ll = List[Expression]();
		  if (start > stop) {
		    for (i <- start to stop) {
		      l ::= nodes(i);
		    }
		    ll = l;
		  }else{
		    for (i <- stop to start) {
		      l ::= nodes(i);
		    }
		    ll = l.reverse;
		  }
		  return ll.reverse;
		}
		
		def get_binary_left_body(nodes: List[Expression], binary_index: Int): Expression = {
		  // On the left from binary expression, only variables are allowed for assignments and also numbers, literals, true, false for others.
		  //no multiple assignments allowed - so there can be only one element on the left (which can be complex element).
		  var n = (nodes.length - 1) - binary_index;
		  if (n <= 0) {
		    return null;
		  }
		  var left_nodes = nodes.dropRight(n);
		  var root = create_local_AST2(left_nodes);
		  return root;
		}
		
		def get_binary_right_body(nodes: List[Expression], binary_index: Int): Expression = {
		  // On the left from binary expression, only variables are allowed for assignments and also numbers, literals, true, false for others.
		  //no multiple assignments allowed - so there can be only one element on the left (which can be complex element).
		  if (binary_index <= 0) {
		    return null;
		  }
		  var right_nodes = nodes.drop(binary_index + 1);   //indexes start from 0
		  var root = create_local_AST2(right_nodes);
		  return root;
		}
		
		def get_right_body(nodes: List[Expression], node_index: Int): Expression = {
		  if (node_index <= 0) {
		    return null;
		  }
		  var right_nodes = nodes.drop(node_index + 1);
		  var root = create_local_AST2(right_nodes);
		  return root;
		}
		
		def get_left_body(nodes: List[Expression], node_index: Int): Expression = {
		  var n = (nodes.length - 1) - node_index;
		  if (n <= 0) {
		    return null;
		  }
		  var left_nodes = nodes.dropRight(n);
		  var root = create_local_AST2(left_nodes);
		  return root;
		}
		
		
		def find_root(nodes: List[Expression]): Expression = {
		  if (nodes == null) {
		    return null;
		  }
		  var node: Expression = null;
		  nodes(0).get_type() match {
		    case "bracket"|"if"|"elif"|"else"|"yield"|"return"|"repeat"|"break"|"loop"|"loop_for_over"|"procedure_definition"|"iterator_definition"|"procedure_call" => node = nodes(0); node.set_line_index(0);
		    case "class" => if (nodes.length > 2 && nodes(2) == "extends") {node = nodes(2); node.set_line_index(2);}else{node = nodes(0); node.set_line_index(0);}
		    case "variable" => node = get_first_of_type("binary_expression", nodes); node.set_line_index(current_first_of_type_index);
		    case "proc"|"iter" => println("definition without reference. ");
		    case _ => println("ERRROR: token unkown. ");
		  }
		  return node;
		}
		
		def give_definition_body(d: Definition, body_start_index: Int, nodes: List[Expression]): Expression = {
		  var def_node = d;
		  var len = nodes.length;
		  if (body_start_index > (len - 1)) {
		    return def_node;
		  }
		  var n = nodes(body_start_index);
		  if (is_curly_openning(n)) {
		    def_node.set_openned(true);
		  }else{
		    def_node.set_openned(false);
		  }
		  var root: Expression = null;
		  if (def_node.get_opened && body_start_index + 1 <= nodes.length - 1) {
		    var nodes_sublist = get_sub_list(nodes, body_start_index + 1, nodes.length - 1);
		    root = create_local_AST2(nodes_sublist);
		  }
		  if (root != null && def_node.get_opened) {
		    def_node.set_body(List[Expression](root));
		  }
		  var last_node_in_line = nodes(nodes.length - 1);
		  if (last_node_in_line.get_type == "bracket" && last_node_in_line.asInstanceOf[Brackets].get_bracket == "}") {
		    def_node.set_openned(false);
		  }
		  //if (def_node.get_opened) {
		 //   enqueue_scope(def_node);
		  //}
		  return def_node;
		}
		
		def create_local_AST2(nodes: List[Expression]): Expression = {
		  if (nodes.length == 0) {
		    return null;
		  }
		  var root: Expression = find_root(nodes);
		  if (root == null) {
		    return null;
		  }
		  var typ = root.get_type();
		  typ match {
		    case "binary_expression" => 
		      var left_body = get_binary_left_body(nodes, root.get_line_index());
		      var right_body = get_binary_right_body(nodes, root.get_line_index());
		      root.asInstanceOf[BinaryExpression].set_left(left_body);
		      root.asInstanceOf[BinaryExpression].set_right(right_body);
		    case "procedure_definition"|"iterator_definition" =>
		      if (root.get_line_index() == 0 && nodes.length >= 3 && nodes(1).get_type() == "bracket" && nodes(1).asInstanceOf[Brackets].get_bracket() == "(") {
		        var param_list_start_index = 1;
		        var param_list_stop_index = get_last_index_of_closing_round_br_until_curly_open(nodes);
		        var param_list = get_sub_list(nodes, (param_list_start_index + 1), (param_list_stop_index - 1));
		        root.asInstanceOf[Definition].set_params(param_list);
		        var body_start_index = param_list_stop_index + 1;
		        var d = give_definition_body(root.asInstanceOf[Definition], body_start_index, nodes);
		        root = d;
		      }else{
		        println("Error: Wrong definition synatx. ");
		      }
		    case _ =>
		  }
		  return root;
		}
		
		def add_to_body_of_correct_scope(local_root: Expression) {
		  var s = dequeue_scope();
		  s.get_type match {
		    case "procedure_definition"|"iterator_definition"|"if"|"elif"|"else"|"loop"|"loop_for_over" =>
		      s.asInstanceOf[Definition].add_to_body(local_root);
		    case _ =>
		    enqueue_scope(s);
		  }
		}
		
		
		def is_assignment_op(n: BinaryExpression): Boolean = {
		  if (n.get_bin_op == ":-" || n.get_bin_op == "=") {
		    return true;
		  }
		  return false;
		}
		
		
		def get_first_binary_operator_after_assignment(nodes: List[Expression]): (BinaryExpression, Int) = {
		  var index = 0;
		  for (n <- nodes) {
		    if (n.get_type == "BinaryExpression") {
		      return (n.asInstanceOf[BinaryExpression], index);
		    }
		    index = index + 1;
		  }
		  return null;
		}
		
		
		
		def get_first_operator_after_assignment(nodes: List[Expression]): (Expression, Int) = {
		  var index = 0;
		  for (n <- nodes) {
		    var typ0 = n.get_type;
		    if (typ0 == "BinaryExpression"||typ0 == "conditional_expression"||typ0 == "relational_expression") {
		      return (n.asInstanceOf[BinaryExpression], index);
		    }
		    index = index + 1;
		  }
		  return null;
		}
		
		
		
		def get_first_binary_assignment(nodes: List[Expression]): (List[Expression], Expression, Int) = {
		  var additional_vars = List[Expression]();
		  var index = 0;
		  for (n <- nodes) {
		    if (n.get_type == "binary_expression") {       //There are only two possibilities - either binary expression or more
		    	if(is_assignment_op(n.asInstanceOf[BinaryExpression])) {					//variables and then binary expression.
		    		return (additional_vars, n, index);
		    	}else{
		    		println("Syntax error: symbol not an assignment operator. ");
		    		return null;
		    	}
		    }else if (n.get_type == "variable") {
		    	additional_vars ::= n;
		    }
		    index = index + 1;
		  }
		  println("Syntax error - statement started with variable - must be an assignment. ");
		  return null;
		}
		
		
		def is_arithmetic_or_concat(nodes: List[Expression]): Boolean = {
		  for (n <- nodes) {
		    var typ = n.get_type;
		    if (typ == "conditional_expression"||typ == "relational_expression"||typ == "true"||typ == "false") {
		      return false;
		    }
		  }
		  return true;
		}
		
		def is_boolean(nodes: List[Expression]): Boolean = {
		  for (n <- nodes) {
		    var typ = n.get_type;
		    if (typ == "conditional_expression"||typ == "relational_expression"||typ == "true"||typ == "false") {
		      return true;
		    }
		  }
		  return false;
		}
		
		def find_meaning(nodes: List[Expression]): Int = {
		  if (is_arithmetic_or_concat(nodes)) {
		    return meanings("arithmetic_or_concat");
		  }else if(is_boolean(nodes)){
		    return meanings("boolean");
		  }else{  //is erroneous.
		    return meanings("sytax_error");
		  }
		}
		
		
		//def get_bracketed_roots(nodes: List[Expression]): List[Expression] = {
		 // var bracketed_roots = List[Expression]();
		  //var open_brackets_indices = Queue[Brackets]();
		  //var close_brackets_indices = Queue[Brackets]();
		  //var curr_root: Expression = null;
		  //for (n <- nodes) {
		    //if (n.get_type == "bracket") {
		      //if (n.asInstanceOf[Brackets].get_bracket == "(") {
		        //
		      //}else if (n.asInstanceOf[Brackets].get_bracket == ")") {
		        //
		      //}
		    //}
		  //}
		//}
		
		
		def parse_arithmetic(nodes: List[Expression]): Expression = {
		  println("NODES:  " + nodes);
		  var root: Expression = null;
		  if (nodes.length == 1) {
		    return nodes(0);
		  }
		  var temp = adjust_signs_and_operators(nodes);
		  println("---------> 1 " + temp);
		  var temp2 = parse_brackets(temp);
		  println("---------> 2 " + temp2);
		  var temp3 = parse_power(temp2);
		  println("---------> 3 " + temp3);
		  //multiplication and devision.
		  var m_d_index = 0;
		  var temp4 = parse_mult_div(temp3);
		  println("---------> 4 " + temp4);
		  var temp5 = List[Expression]();
		  if (temp4.length > 0) {
		    temp5 = parse_add_subt(temp4);
		    println("---------> 5 " + temp5);
		  }
		  if (temp5.length == 1) {
		    root = temp5(0);
		  }else{
		    Console.err.println("ERROR: uknown tokens used inside an expression. ");
		  }
		  return root;
		}
		
		
		//This procedure has to be done away with or implemented later. For the moment
		//there is no way to use minus numbers. It is only a "STUB" at present.
		def adjust_signs_and_operators(tokens: List[Expression]): List[Expression] = {
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var ret_tokens = List[Expression]();
		  for (index <- 0 to (tokens.length - 1)) {
		    var n = tokens(index);
		    var t = n.get_type;
		    if (t == "binary_expression") {
		      if (index == 0 && tokens(index + 1).isInstanceOf[Signable] && n.asInstanceOf[BinaryExpression].get_bin_op == "-") {
		        tokens(index + 1).asInstanceOf[Signable].update_sign();
		      }else if (index > 0 && tokens(index + 1).isInstanceOf[Signable] && n.asInstanceOf[BinaryExpression].get_bin_op == "-" && (tokens(index - 1).get_type == "binary_expression" || tokens(index - 1).get_type == "relational_expression" || tokens(index - 1).get_type == "conditional_expression")) {
		        tokens(index + 1).asInstanceOf[Signable].update_sign();
		      }else{
		        ret_tokens ::= n;
		      }
		    }else{
		      ret_tokens ::= n;
		    }
		  }
		  return ret_tokens.reverse;
		}
		
		
		
		def get_next_open_round_index(start: Int, nodes: List[Expression]): Int = {
		  var index = start;
		  for (n <- nodes) {
		    if (n.get_type == "bracket") {
		      if (n.asInstanceOf[Brackets].get_bracket == "(") {
		        return index;
		      }
		    }
		    index = index + 1;
		  }
		  return -1;
		}
		
		
		
		def adjust_signs_and_operators2(tokens: List[Expression]): List[Expression] = {
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var ret_tokens = List[Expression]();
		  var binary_index = -1;
		  var index = 0;
		  var skip = false;
		  for (n <- tokens) {
		    var n_type = n.get_type;
		    if (binary_index == -1 && (index != 0 || n_type != "binary_expression") && !skip) {
		      ret_tokens ::= n;
		    }
		    if (binary_index != -1 && n_type != "binary_expression") {
		      ret_tokens ::= n;
		    }
		    if (n_type == "binary_expression" && binary_index == -1 && index != 0) {
		      binary_index = index;
		    }else if (n_type == "binary_expression" && (binary_index > -1 || index == 0)) {
		      if (n.asInstanceOf[BinaryExpression].get_bin_op == "-" && index < tokens.length - 1) {
		        var node = tokens(index + 1);
		        var t = node.get_type;
		        t match {
		          case "variable" => node.asInstanceOf[VariableExpression].set_sign("-");
		          case "number" => node.asInstanceOf[NumberExpression].set_sign("-");
		          case "iterator_call" => node.asInstanceOf[IteratorCallingExpression].set_sign("-");
		          case "procedure_call" => node.asInstanceOf[ProcedureCallingExpression].set_sign("-");
		          case _ =>;
		        }
		        ret_tokens ::= node;
		        skip = true;
		      }else{
		        Console.err.println("ERROR: " + n.asInstanceOf[BinaryExpression].get_bin_op + "token not allowed in this place. ");
		      }
		      binary_index = -1;
		    }
		    if (n_type != "binary_expression") {
		      skip = false;
		    }
		    index = index + 1;
		  }
		  if (ret_tokens.length == 0) {
		    return tokens;
		  }
		  return ret_tokens.reverse;
		}
		
		
		def parse_brackets(tokens: List[Expression]): List[Expression] = {
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var ret_tokens = List[Expression]();
		  var openned_bracks = Stack[Int]();
		  var curr_root_from_br: Expression = null;
		  var index = 0;
		  var close_index = -1;
		  for (n <- tokens) {
		    var popped = -1;
		    if (n.get_type == "bracket") {
		      if (n.asInstanceOf[Brackets].get_bracket() == "(") {
		        openned_bracks.push(index);
		      }
		      if (n.asInstanceOf[Brackets].get_bracket() == ")") {
		        popped = openned_bracks.pop;
		        close_index = index;
		      }
		    }else if (n.get_type != "bracket" && openned_bracks.isEmpty) {
		      if (popped > -1) {
		        var sublist = tokens.drop(popped + 1).dropRight(tokens.length - close_index);
		        var node = parse_arithmetic(sublist);
		        ret_tokens ::= node;
		      }
		      ret_tokens ::= n;
		    }
		    index = index + 1;
		  }
		  if (ret_tokens.length == 0) {
		    return tokens;
		  }
		  return ret_tokens.reverse;
		}
		
		
		def parse_power(tokens: List[Expression]): List[Expression] = {
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var ret_tokens = List[Expression]();
		  //var index = 0;
		  var len = tokens.length;
		  for (index <- 0 to (len - 1)) {
		    var n = tokens(index);
		    var t = n.get_type;
		    if (t == "binary_expression" && n.asInstanceOf[BinaryExpression].get_bin_op == "^") {
		      var temp = ret_tokens(0);
		      var temp_p = temp.get_parent;
		      var temp_ret = ret_tokens.drop(1);
		      n.set_parent(temp_p);
		      n.asInstanceOf[BinaryExpression].set_left(temp);
		      temp.set_parent(n);
		      n.asInstanceOf[BinaryExpression].set_right(tokens(index + 1));
		      tokens(index + 1).set_parent(n);
		      ret_tokens = temp_ret;
		      ret_tokens ::= n;
		    }else if (index == 0 || (index > 0 && (tokens(index - 1).get_type != "binary_expression" || (tokens(index - 1).get_type == "binary_expression" && tokens(index - 1).asInstanceOf[BinaryExpression].get_bin_op != "^")))) {
		      ret_tokens ::= n;
		    }
		  }
		  return ret_tokens.reverse;		  
		}
		

		
		
		def parse_mult_div(tokens: List[Expression]): List[Expression] = {
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var ret_tokens = List[Expression]();
		  var len = tokens.length;
		  for (index <- 0 to (len - 1)) {
		    var n = tokens(index);
		    var t = n.get_type;
		    if (t == "binary_expression" && (n.asInstanceOf[BinaryExpression].get_bin_op == "*" || n.asInstanceOf[BinaryExpression].get_bin_op == "/")) {
		      var temp = ret_tokens(0);
		      var temp_p = temp.get_parent;
		      var temp_ret = ret_tokens.drop(1);
		      n.set_parent(temp_p);
		      n.asInstanceOf[BinaryExpression].set_left(temp);
		      temp.set_parent(n);
		      n.asInstanceOf[BinaryExpression].set_right(tokens(index + 1));
		      tokens(index + 1).set_parent(n);
		      ret_tokens = temp_ret;
		      ret_tokens ::= n;
		    }else if (index == 0 || (index > 0 && (tokens(index - 1).get_type != "binary_expression" || (tokens(index - 1).get_type == "binary_expression" && (tokens(index - 1).asInstanceOf[BinaryExpression].get_bin_op != "*" && tokens(index - 1).asInstanceOf[BinaryExpression].get_bin_op != "/"))))) {
		      ret_tokens ::= n;
		    }
		  }
		  return ret_tokens.reverse;
		}
		
		
		def parse_add_subt(tokens: List[Expression]): List[Expression] = {
		  println("tokens: " + tokens);
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var ret_tokens = List[Expression]();
		  var len = tokens.length;
		  for (index <- 0 to (len - 1)) {
		    var n = tokens(index);
		    var t = n.get_type;
		    if (t == "binary_expression" && (n.asInstanceOf[BinaryExpression].get_bin_op == "+" || n.asInstanceOf[BinaryExpression].get_bin_op == "-")) {
		      var temp = ret_tokens(0);
		      var temp_p = temp.get_parent;
		      var temp_ret = ret_tokens.drop(1);
		      n.set_parent(temp_p);
		      n.asInstanceOf[BinaryExpression].set_left(temp);
		      temp.set_parent(n);
		      n.asInstanceOf[BinaryExpression].set_right(tokens(index + 1));
		      tokens(index + 1).set_parent(n);
		      ret_tokens = temp_ret;
		      ret_tokens ::= n;
		    }else if (index == 0 || (index > 0 && (tokens(index - 1).get_type != "binary_expression" || (tokens(index - 1).get_type == "binary_expression" && (tokens(index - 1).asInstanceOf[BinaryExpression].get_bin_op != "+" && tokens(index - 1).asInstanceOf[BinaryExpression].get_bin_op != "-"))))) {
		      ret_tokens ::= n;
		    }
		  }
		  return ret_tokens.reverse;
		}
		
		
		def parse_boolean(nodes: List[Expression]): Expression = {
		  println("                      <<<<  " + nodes + "  >>>>");
		  var root: Expression = null;
		  if (nodes.length == 1) {
		    return nodes(0);
		  }
		  var temp0 = get_cleared_nots(nodes);
		  println("0                 <<  " + temp0 + " >>");
		  var temp1 = adjust_signs_and_operators(temp0);
		  println("1                 <<  " + temp1 + " >>");
		  var temp2 = resolve_brackets_for_boolean_eval(temp1);
		  println("2                 <<  " + temp2 + " >>");
		  var temp3 = parse_power(temp2);
		  println("3                 <<  " + temp3 + " >>");
		  var temp4 = parse_mult_div(temp3);
		  println("4                 <<  " + temp4 + " >>");
		  var temp5 = parse_add_subt(temp4);
		  println("5                 <<  " + temp5 + " >>");
		  var temp6 = parse_relations(temp5);
		  println("6                 <<  " + temp6 + " >>");
		  if (temp6.length == 1) {
		    println("  > " + temp6(0).asInstanceOf[RelationalExpression].get_left.get_type + " : " + temp6(0).asInstanceOf[RelationalExpression].get_right.get_type);
		  }
		  var temp7 = parse_conditionals(temp6);
		  println("7                 <<  " + temp7 + " >>");
		  if (temp7.length == 1) {
		    root = temp7(0);
		  }else{
		    Console.err.println("ERROR: Tokens - not recogonised. ");
		  }
		  return root; 
		}
		
		 
		def parse_conditionals(tokens: List[Expression]): List[Expression] = {
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var ret_tokens = List[Expression]();
		  var skip = false;
		  for (index <- 0 to (tokens.length - 1)) {
		    var n = tokens(index);
		    var t = n.get_type;
		    if (t != "conditional_expression") {
		      if (!skip) {
		        ret_tokens ::= n;
		      }else{
		        skip = false;
		      }
		    }else{
		      var temp = ret_tokens(0);
		      var temp_p = temp.get_parent;
		      var temp_ret = ret_tokens.drop(1);
		      ret_tokens = temp_ret;
		      n.asInstanceOf[ConditionOperatorExpression].set_left(temp);
		      temp.set_parent(n);
		      n.asInstanceOf[ConditionOperatorExpression].set_right(tokens(index + 1));
		      tokens(index + 1).set_parent(n);
		      n.set_parent(temp_p);
		      ret_tokens ::= n;
		      skip = true;
		    }
		  }
		  return ret_tokens.reverse;
		}
		
		
		def parse_relations(tokensx: List[Expression]): List[Expression] = {
		  if (tokensx.length == 1) {
		    return tokensx;
		  }
		  var tokens = List[Expression]();
		  var found = false;
		  for (ind <- 0 to (tokensx.length - 1)) {
		    var n = tokensx(ind);
		    var t = n.get_type;
		    if (t != "relational_expression" || (t == "relational_expression" && n.asInstanceOf[RelationalExpression].get_operator != "~")) {
		      if (!found) {
		        tokens ::= n;
		      }else{
		        found = false;
		      }
		    }else{
		      n.asInstanceOf[RelationalExpression].set_left(null);
		      n.asInstanceOf[RelationalExpression].set_right(tokensx(ind + 1))
		      tokensx(ind + 1).set_parent(n);
		      tokens ::= n;
		      found = true;
		    }
		  }
		  tokens = tokens.reverse;
		  
		  var ret_tokens2 = List[Expression]();
		  var skip = false;
		  for (index <- 0 to (tokens.length - 1)) {
		    var n = tokens(index);
		    var t = n.get_type;
		    if (t != "relational_expression") {
		      if (!skip) {
		        ret_tokens2 ::= n;
		      }else{
		        skip = false;
		      }
		    }else{
		      var temp = ret_tokens2(0);
		      var temp_p = temp.get_parent;
		      var temp_ret2 = ret_tokens2.drop(1);
		      ret_tokens2 = temp_ret2;
		      n.set_parent(temp_p);
		      n.asInstanceOf[RelationalExpression].set_left(temp);
		      temp.set_parent(n);
		      n.asInstanceOf[RelationalExpression].set_right(tokens(index + 1));
		      tokens(index + 1).set_parent(n);
		      ret_tokens2 ::= n;
		      skip = true;
		    }
		  }
		  return ret_tokens2.reverse;
		}
		
		
		def resolve_brackets_for_boolean_eval(tokens: List[Expression]): List[Expression] = {
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var ret_tokens = List[Expression]();
		  var openned_bracks = Stack[Int]();
		  var curr_root_from_br: Expression = null;
		  var index = 0;
		  var close_index = -1;
		  for (n <- tokens) {
		    var popped = -1;
		    if (n.get_type == "bracket") {
		      if (n.asInstanceOf[Brackets].get_bracket() == "(") {
		        openned_bracks.push(index);
		      }
		      if (n.asInstanceOf[Brackets].get_bracket() == ")") {
		        popped = openned_bracks.pop;
		        close_index = index;
		      }
		    }else if (n.get_type != "bracket" && openned_bracks.isEmpty) {
		      if (popped > -1) {
		        var sublist = tokens.drop(popped + 1).dropRight(tokens.length - close_index);
		        var node = parse_boolean(sublist);
		        ret_tokens ::= node;
		      }
		      ret_tokens ::= n;
		    }
		    index = index + 1;
		  }
		  if (ret_tokens.length == 0) {
		    return tokens;
		  }
		  return ret_tokens.reverse;
		}
		
		
		def get_cleared_nots(tokens: List[Expression]): List[Expression] = {
		  if (tokens.length == 1) {
		    return tokens;
		  }
		  var processed_tokens = List[Expression]();
		  var ret_tokens = List[Expression]();
		  var index = 0;
		  var curr_root_bl: RelationalExpression = null;
		  var skip = false;
		  var index1 = 0;
		  for (n <- tokens) {
		    if (!skip) {
		      processed_tokens ::= n;
		    }else{
		      skip = false;
		    }
		    if (n.get_type == "conditional_expression") {
		      var op = n.asInstanceOf[ConditionOperatorExpression].get_operator;
		      if (op == "~" && index == tokens.length - 1) {
		        Console.err.println("ERROR: no expression can end with: " + op);
		      }else if (op == "~" && index1 < tokens.length - 1) {
		        skip = true;
		        var notted_node = tokens(index + 1);
		        n.asInstanceOf[ConditionOperatorExpression].set_left(null);
		        n.asInstanceOf[ConditionOperatorExpression].set_right(notted_node);
		        notted_node.set_parent(n);
		        processed_tokens ::= n;
		      }
		    }
		    index1 = index1 + 1;
		  }
		  if (processed_tokens.length == 0) {
		    return tokens;
		  }
		  return processed_tokens.reverse;
		}
		
		
		
		def parse_return(nodes: List[Expression]): Expression = {
		  if (nodes.length == 1) {
		    return nodes(0);
		  }
		  var root: Expression = null;
		  root = nodes(0);
		  var body = List[Expression]();
		  var sublist = nodes.drop(1);
		  var first = sublist(0);
		  var ft = first.get_type;
		  var end = sublist(sublist.length - 1);
		  var endt = end.get_type;
		  if (sublist.length == 1) {
		    println(" <*> " + nodes + " <!> ");
		    root.asInstanceOf[ReturnExpression].set_body(sublist);
		    sublist(0).set_parent(root);
		  }
		  if (is_arithmetic_or_concat(sublist)) {
		    var n = parse_arithmetic(sublist);
		    root.asInstanceOf[ReturnExpression].set_body(List[Expression](n));
		    n.set_parent(root);
		  }else if (is_boolean(sublist)) {
		    var n = parse_boolean(sublist);
		    root.asInstanceOf[ReturnExpression].set_body(List[Expression](n));
		    n.set_parent(root);
		  }
		  return root;
		}
		
		
		
		def get_right_after_assignment_on_variable_first(nodes: List[Expression]): Expression = {
		  println("Showing nods: " + nodes);
		  var len = nodes.length;
		  var root: Expression = null;
		  var meaning = find_meaning(nodes);
		  println(meaning);
		  meaning match {
		    case 1 =>
		      root = parse_arithmetic(nodes);
		    case 2 =>
		      root = parse_boolean(nodes);
		    case 3 =>
		      println("Can't parse - syntax error = uknown expression. ");
		    case _ =>
		  }
		  return root;
		}
		
		
		
		def	get_right_root_of_binary_op(right_nodes: List[Expression]): Expression = {
		  var right_last = right_nodes.length - 1;
		  var first_node = right_nodes(0);
		  var typ0 = first_node.get_type;
		  var root: Expression = null;
		  println("Length is now : " + right_nodes.length + " : " + right_nodes);
		  typ0 match {
		    case "number" =>
		      if (right_nodes.length == 1) {
		        root = first_node;
		      }else{
		        var node = parse_arithmetic(right_nodes);
		        root = node;
		      }
		    case "string"|"character" =>
		      if (right_nodes.length == 1) {
		        root = first_node;
		      }
		    case "procedure_call"|"iterator_call" =>
		      root = first_node;
		    case "procedure_definition"|"iterator_definition" =>
		      root = first_node;
		      next_possible_scope = root;
		      if (right_nodes(right_nodes.length - 1).get_type == "bracket") {
		        if (right_nodes(right_nodes.length - 1).asInstanceOf[Brackets].get_bracket == "{") {
		          root.asInstanceOf[Opennable].set_openned(true);
		        }
		      }
		      var op_ind = get_first_index_of_open_round_br(right_nodes);
		      var cl_ind = get_last_index_of_closing_round_br_until_curly_open(right_nodes);
		      if (cl_ind - op_ind > 1) {
		        var sublist = right_nodes.drop(op_ind + 1).dropRight(right_nodes.length - cl_ind);
		        root.asInstanceOf[Definition].set_params(sublist);
		      }
		      println("OP: " + op_ind + " | " + "CLOSE_IND: " + cl_ind);
		      println("Rnodes: " + right_nodes);
		      println("proccccccccccccccc >>>>   " + root.asInstanceOf[Definition].get_parameters + "   :   " + root.asInstanceOf[Definition].get_opened + " : " + root.asInstanceOf[Definition].get_body);
		    case "variable" =>
		      if (right_nodes.length == 1) {
		        root = first_node;
		      }else{
		        root = get_right_after_assignment_on_variable_first(right_nodes);
		      }
		    case "bracket" => 
		      var n = right_nodes(0);
		      n.asInstanceOf[Brackets].get_bracket match {
		        case "{" =>  
		          if (next_possible_scope != null && next_possible_scope.is_opennable && !next_possible_scope.asInstanceOf[Opennable].get_opened) {
		            next_possible_scope.asInstanceOf[Opennable].set_openned(true);
		          }
		        case "}" =>
		          close_scope = true;
		        case _ =>
		      }
		    case _ =>
		  }
		  return root;
		}
		
		
		def get_index_of(l: List[Any], elem: Any): Int = {
		  var index = 0;
		  for (e <- l) {
		    if (e == elem) {
		      return index;
		    }
		    index = index + 1;
		  }
		  return -1;
		}
		
		
		def get_index_of_iter_call(l: List[Expression]): Int = {
		  var index = 0;
		  for (x <- l) {
		    if (x.get_type == "iterator_call") {
		      return index;
		    }
		    index = index + 1;
		  }
		  return -1;
		}
		
		
		def get_root_local_AST(nodes: List[Expression]): Expression = {
		  if (nodes.length == 0) {
		    return null;
		  }
		  var first_of_nodes = nodes(0);
		  var typ0 = first_of_nodes.get_type;
		  var root: Expression = null;
		  typ0 match {
		    case "while_loop" =>
		      var node: WhileExpression = first_of_nodes.asInstanceOf[WhileExpression];
		      root = first_of_nodes;
		      next_possible_scope = root;
		      var sublist = nodes.drop(1);
		      if (nodes(nodes.length - 1).get_type == "bracket") {
		        if (nodes(nodes.length - 1).asInstanceOf[Brackets].get_bracket == "{") {
		          next_possible_scope.asInstanceOf[Opennable].set_openned(true);
		          sublist = sublist.dropRight(1);
		        }
		      }
		      var condition = parse_boolean(sublist);
		      node.set_condition(condition.asInstanceOf[Booleanable]);
		      condition.set_parent(node);
		    case "class" =>
		      var node: ClassExpression = first_of_nodes.asInstanceOf[ClassExpression];
		      root = first_of_nodes;
		      next_possible_scope = root;
		      println("N-O-D-E-S: " + nodes);
		      if (nodes(nodes.length - 1).get_type == "bracket") {
		        if (nodes(nodes.length - 1).asInstanceOf[Brackets].get_bracket == "{") {
		          next_possible_scope.asInstanceOf[Opennable].set_openned(true);
		        }
		      }
		    case "procedure_call" =>
		      var sublist = nodes.drop(1).dropRight(1);
		      first_of_nodes.asInstanceOf[ProcedureCallingExpression].set_arguments(sublist);
		      root = first_of_nodes;
		    case "loop"|"else" => 
		      if (nodes.length > 2) {
		        Console.err.println("Error: Uknown tokens. ");
		      }else if (nodes.length == 2 && nodes(1).get_type != "bracket") {
		        Console.err.println("Error: Uknown tokens. Openning bracket \"{\" is missing. ");
		      }else if (nodes.length == 2 && nodes(1).get_type == "bracket" && nodes(1).asInstanceOf[Brackets].get_bracket != "{") {
		        Console.err.println("Error: Uknown tokens. Openning bracket \"{\" is missing. ");
		      }else if (nodes.length == 2 && nodes(1).get_type == "bracket" && nodes(1).asInstanceOf[Brackets].get_bracket == "{") {
		        root = first_of_nodes;
		        root.asInstanceOf[Opennable].set_openned(true);
		        next_possible_scope = root;
		      }else if (nodes.length == 1) {
		        root = first_of_nodes;
		        root.asInstanceOf[Opennable].set_openned(false);
		        next_possible_scope = root;
		      }
		    case "if"|"elif" =>
		      if (nodes.length == 1) {
		        Console.err.println("Error: Expression is not recognised. ");
		      }else if (nodes.length > 2 && nodes(nodes.length - 1).get_type == "bracket" && nodes(nodes.length - 1).asInstanceOf[Brackets].get_bracket == "{") {
		        first_of_nodes.asInstanceOf[Opennable].set_openned(true);
		        var sublist = nodes.drop(1).dropRight(1);
		        var node = parse_boolean(sublist);
		        if (typ0 == "if") {
		          first_of_nodes.asInstanceOf[IfExpression].set_condition(node);
		          println("..........  >>>>  " + node);
		          node.set_parent(first_of_nodes);
		        }else if (typ0 == "elif") {
		          first_of_nodes.asInstanceOf[ElifExpression].set_condition(node);
		          node.set_parent(first_of_nodes);
		        }
		        root = first_of_nodes;
		        next_possible_scope = root;
		      }else if (nodes.length > 1 && nodes(nodes.length - 1).get_type != "bracket") {
		        root = first_of_nodes;
		        root.asInstanceOf[Opennable].set_openned(false);
		        next_possible_scope = root;
		      }
		    case "loop_for_over" =>
		      var iter_ind = get_index_of_iter_call(nodes);
		      var iter = nodes(iter_ind);
		      var lst_cl_rbr = get_last_index_of_closing_round_br_until_curly_open(nodes);
		      var sublist = nodes.drop(iter_ind + 2).dropRight(nodes.length - lst_cl_rbr);
		      var ret_vars = nodes.drop(1).dropRight(nodes.length - iter_ind);
		      iter.asInstanceOf[IteratorCallingExpression].set_arguments(sublist);
		      iter.set_parent(first_of_nodes);
		      first_of_nodes.asInstanceOf[LoopForExpression].set_iterator_vars(ret_vars);
		      root = first_of_nodes;
		      if (nodes(nodes.length - 1).get_type == "bracket" && nodes(nodes.length - 1).asInstanceOf[Brackets].get_bracket == "{") {
		        first_of_nodes.asInstanceOf[Opennable].set_openned(true);
		      }else{
		        first_of_nodes.asInstanceOf[Opennable].set_openned(false);
		      }
		      next_possible_scope = root;
		    case "variable" =>
		      var tuple3 = get_first_binary_assignment(nodes.drop(1));
		      if (tuple3 != null) {
		        root = tuple3._2.asInstanceOf[BinaryExpression]; 
		        tuple3._1.foreach(_.set_parent(root));
		        root.asInstanceOf[BinaryExpression].set_multiple_left(tuple3._1);
		        root.asInstanceOf[BinaryExpression].set_left(first_of_nodes);
		        first_of_nodes.set_parent(root);
		      }
		      var index = tuple3._3;
		      var right_root = get_right_root_of_binary_op(nodes.drop(index + 2));
		      println(">>>> " + right_root);
		      root.asInstanceOf[BinaryExpression].set_right(right_root);
		      right_root.set_parent(root);
		    case "bracket" =>
		      root = first_of_nodes;
		      var br = first_of_nodes.asInstanceOf[Brackets].get_bracket;
		      if (br == "{") {
		        if (next_possible_scope != null && next_possible_scope.is_opennable && !next_possible_scope.asInstanceOf[Opennable].get_opened) {
		          next_possible_scope.asInstanceOf[Opennable].set_openned(true);
		        }
		      }else if (br == "}") {
		        var conditional = false;
		        if (nodes.length > 1) {
		          var second_node = nodes(1);
		          var last_node = nodes(nodes.length - 1);
		          var sc = head_scope();
		          second_node match {
		            case x: ElifExpression => sc.asInstanceOf[MultBody].add_to_body(second_node); conditional = true;
		            case x: ElseExpression => sc.asInstanceOf[MultBody].add_to_body(second_node);
		            case _ =>
		          }
		          var sublist: List[Expression] = null;
		          last_node match {
		            case x: ClosedCurlyBracket => sublist = nodes.drop(1).dropRight(1);
		            case _ => sublist = nodes.drop(1);
		          }
		          conditional match {
		            case true => second_node.asInstanceOf[ElifExpression].set_condition(parse_boolean(sublist));
		            case _ =>
		          }
		          dequeue_scope();
		          last_node match {
		            case x: OpenCurlyBracket => second_node.asInstanceOf[Opennable].set_openned(true);
		          }
		          root = second_node;
		          next_possible_scope = root;
		        }
		      }
		    case "return" =>
		      println("N-O-D-E-S: " + nodes);
		      root = parse_return(nodes);
		    case _ =>
		  }
		  return root;
		}
		
		
		def create_AST(scanner: Scanner) {
			scanner.start_scann();
			//var value_list = scanner.nextLine();
			var nodes: List[Expression] = null;
			do {
				nodes = scanner.nextLine();
				var local_root: Expression = null;
				if (nodes != null) {
					local_root = get_root_local_AST(nodes);
				}
				if (local_root != null) {
				  println("======> " + local_root.get_type);
				  update_tree(local_root);
				}
				if (close_scope) {
				  var r = dequeue_scope();
		          r.asInstanceOf[Opennable].set_openned(false);
		          close_scope = false;
				}
			}while(nodes != null);
			scanner.close;
		}
		
		def update_tree(n: Expression) {
		  var sc = head_scope();
		  var sc_type = sc.get_type();
		  var t = n.get_type;
		  if (sc.has_multi_body) {
		    if (t != "bracket") {
		      sc.asInstanceOf[MultBody].add_to_body(n);
		      n.set_parent(sc); 
		    }
		  }
		  if (t == "bracket" && n.asInstanceOf[Brackets].get_bracket == "}") {
		    var r = dequeue_scope();
		    r.asInstanceOf[Opennable].set_openned(false);
		    return;
		  }
		  //enqueue_scope(sc);
		  if (next_possible_scope != null && next_possible_scope.is_opennable && next_possible_scope.asInstanceOf[Opennable].get_opened) {
		    enqueue_scope(next_possible_scope);
		    next_possible_scope = null;
		  }
		}
		
		
		def show_AST(root: Expression, level: Int) {
		  print(level + " : ");
		  if (root != null && root.isInstanceOf[MultBody]) {
		    var body = root.asInstanceOf[MultBody].get_body;
		    if (body != null) {
		      for (kid <- body) {
		        print(kid.get_type + "|")
		        //show_AST(kid, level + 1)
		      }
		      println();
		      println();
		      for (kid <- body) {
		        if (kid != null && kid.isInstanceOf[MultBody]) {
		          println(kid.get_type + " :");
		          var body2 = kid.asInstanceOf[MultBody].get_body;
		          if (body2 != null) {
		            for (k <- body2) {
		              println("       " + k.get_type);
		            }
		          }
		        }else if (kid != null && kid.isInstanceOf[SingleBody]) {
		          println(kid.get_type + " :");
		          var left = kid.asInstanceOf[SingleBody].get_left;
		          var right = kid.asInstanceOf[SingleBody].get_right;
		          if (left != null) {
		            print("       " + left.get_type);
		            if (left.isInstanceOf[SingleBody]) {
		              print("<>" + left.asInstanceOf[SingleBody].get_left + "<>" + left.asInstanceOf[SingleBody].get_right);
		            }
		          }
		          if (right != null) {
		            print("       " + right.get_type);
		            if (right.isInstanceOf[SingleBody]) {
		              print("<>" + right.asInstanceOf[SingleBody].get_left + "<>" + right.asInstanceOf[SingleBody].get_right);
		            }
		          }
		        }
		        println();
		      }
		    }
		    println();
		    show_loops_dip(body);
		  }
		}
		
		
		def show_loops_dip(body: List[Expression]) {
		  println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
		  println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
		  println(">>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>");
		  var bodyx = rootAST.asInstanceOf[GlobalExpression].get_body;
		  for (x <- bodyx) {
		    if (x.get_type == "class") {
		      println("CL: > " + x.get_type + " " + x.asInstanceOf[ClassExpression].get_class_name);
		      var kids = x.asInstanceOf[ClassExpression].get_body;
		      println("   >>>  " + kids + "  <<<   ");
		      for (kid <- kids) {
		        if (kid.get_type == "binary_expression") {
		          var left = kid.asInstanceOf[BinaryExpression].get_left;
		          var right = kid.asInstanceOf[BinaryExpression].get_right;
		          if (left != null) {
		            print("       " + left.get_type);
		          }else{
		            println("LEFT doesn't exist. ");
		          }
		          if (right != null) {
		            print("       " + right.get_type);
		            if (right.get_type == "procedure_definition") {
		              var body_proc = right.asInstanceOf[Definition].get_body;
		              println();
		              var ind = 0;
		              for (bp <- body_proc) {
		                if (ind == 0) {
		                  print("              " + bp.get_type + "|");
		                }else{
		                  print(bp.get_type + "|");
		                }
		                ind = ind + 1;
		              }
		              println();
		              var temp_bs = List[Expression]();
		              for (bp <- body_proc) {
		                if (bp.get_type == "if") {
		                  temp_bs = bp.asInstanceOf[IfExpression].get_body;
		                  println("kids of if: " + temp_bs);
		                }
		              }
		              for (b <- temp_bs if b.get_type == "binary_expression") {
		                println("kids of binary_expression inside if: " + b.asInstanceOf[BinaryExpression].get_left + " : " + b.asInstanceOf[BinaryExpression].get_right);
		                if (b.asInstanceOf[BinaryExpression].get_right.get_type == "binary_expression") {
		                  print(" > " + b.asInstanceOf[BinaryExpression].get_right.asInstanceOf[BinaryExpression].get_left + " : " + b.asInstanceOf[BinaryExpression].get_right.asInstanceOf[BinaryExpression].get_right);
		                }
		                println();
		              }
		            }
		            println();
		          }else{
		            println("RIGHT doeesn't exist. ");
		          }
		          println();
		        }else{
		          println("it is not binary node: " + kid.get_type);
		        }
		      }
		    }
		  }
		  println();
		  println("-----------------------------------------------------------------------");
		  println("-----------------------------------------------------------------------");
		  
		  var binaries = List[BinaryExpression]();
		  for (b <- body) {
		    if (b.get_type == "loop" || b.get_type == "loop_for_over") {
		      println("> " + b.get_type);
		      var body2 = b.asInstanceOf[FlowControlExpression].get_body;
		      if (body2 != null) {
		        for (bb <- body2 if bb.get_type == "binary_expression") {
		          var left = bb.asInstanceOf[BinaryExpression].get_left;
		          var right = bb.asInstanceOf[BinaryExpression].get_right;
		          if (left != null) {
		            print("       " + left.get_type);
		            if (left.get_type == "binary_expression") {
		              var l = left.asInstanceOf[BinaryExpression].get_left;
		              if (l != null) {
		                print(">" + l.get_type + "<");
		              }
		              var r = left.asInstanceOf[BinaryExpression].get_right;
		              if (r != null) {
		                print(">" + r.get_type + "<");
		              }
		            }
		          }
		          if (right != null) {
		            print("       " + right.get_type);
			        if (right.get_type == "binary_expression") {
		              var l = right.asInstanceOf[BinaryExpression].get_left;
		              if (l != null) {
		                print(">" + l.get_type + "<");
		              }
		              var r = right.asInstanceOf[BinaryExpression].get_right;
		              if (r != null) {
		                print(">" + r.get_type + "<");
		              }
		            }
		          }
		        } 
		      }
		    }
		    println();
		  }
		}
		
		
		def get_AST_root(): Expression = {
		  return rootAST;
		}
		
		
		def parse() {
			println("HooshkaScript starts execution! ");
			println();
			create_AST(scanner);
			show_AST(rootAST, 0);
			//test_run(scanner);
			println();
			println("HooshkaScript executed and ended succesfully. ");
		}
	}
}