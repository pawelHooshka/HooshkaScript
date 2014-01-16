package generator {

	import scanner.ast.Expression
	import scanner.ast.MultBody
	import io.FileWriter
	import scanner.ast.BinaryExpression
	import scanner.ast.VariableExpression
	import scanner.ast.NumberExpression
	import scanner.ast.StringExpression
	import scala.collection.mutable.Map;
	import scanner.ast.CharacterExpression
  

	trait LLVM_coder {
	  
	  var current_prefix = "@";
	  
	  val llvm_types = Map[String, String]("character" -> "i8", "string" -> "i8*", "int" -> "i32", "boolean" -> "i1", "real" -> "double");
	  val globals_map = Map[String, String]();
	  
	  
	  /*def emit_ssa_boolean_literal(): String {
	    
      }
  
      def emit_ssa_bracket(): String {
			
	  }
  
	  def emit_ssa_yield(): String {
			
      }
  
	  def emit_ssa_return(): String {
			
      }
		
	  def emit_ssa_repeat(): String {
		    
	  }
  
	  def emit_ssa_break(): String {
	    
	  }
  
	  def emit_ssa_character(): String {
	    
	  }
	  
	  def emit_ssa_string(): String {
	    
	  }
	  
	  def emit_ssa_number(): String {
	    
	  }
	  
	  def emit_ssa_binary_expr(): String {
	    
	  }
	  
	  def emit_ssa_conditional_expr(): String {
	    
	  }
	  
	  def emit_ssa_relational_expr(): String {
	    
	  }
	  
	  def emit_ssa_variable(): String {
	    
	  }
	  
	  def emit_ssa_loop(): String {
	    
	  }
	  
	  def emit_ssa_lfo(): String {
	    
	  }
	  
	  def emit_ssa_if(): String {
	    
	  }
	  
	  def emit_ssa_elif(): String {
	    
	  }
	  
	  def emit_ssa_else(): String {
	    
	  }
	  
	  def emit_ssa_procedure_def(): String {
	    
	  }
	  
	  def emit_ssa_procedure_call(): String {
	    
	  }
	  
	  def emit_ssa_iterator_def(): String {
	    
	  }
	  
	  def emit_ssa_iterator_call(): String {
	    
	  }
	  
	  def emit_ssa_global(): String {
	    
	  }
	  
	  def emit_ssa_extends(): String {
	    
	  }
	  
	  def emit_ssa_class(): String {
	    
	  }*/
	  
	  def get_end_of_binary_chain(n: BinaryExpression): Expression = {
	    var left = n.get_left;
	    var right = n.get_right;
	    if (right.get_type == "binary_expression") {
	      return get_end_of_binary_chain(right.asInstanceOf[BinaryExpression]);
	    }else if (left.get_type == "binary_expression") {
	      return get_end_of_binary_chain(left.asInstanceOf[BinaryExpression]);
	    }else{
	      return n.get_right;
	    }
	  }
	  
	  def get_number_value_str(n: NumberExpression): String = {
	    var t = n.get_number_type;
	    t match {
	      case "int" => return n.get_integer_part.toString;
	      case _ => return n.get_integer_part.toString + "." + n.get_decimal_part.toString;
	    }
	  }
	  
	  def get_string_value(n: StringExpression): String = {
	    return " c\"" + n.asInstanceOf[StringExpression].get_value + "\\0A\\00" + "\"";
	  }
	  
	  def get_string_type(n: StringExpression): String = {
	    return "[" + (n.asInstanceOf[StringExpression].get_value.length() + 2).toString + " x " + llvm_types("character") + "]";
	  }
	  
	  def get_back_refrence_for_string(n: StringExpression, prefix: String, name: String): String = {
	    return "[" + (n.asInstanceOf[StringExpression].get_value.length() + 2).toString + " x " + llvm_types("character") + "]* " + prefix + name + ", ";
	  }
	  
	  def set_globals(x: MultBody, writer: FileWriter) {
	    val prefix = "@";
	    val globals = x.get_body;
	    for (n <- globals if n.get_type == "binary_expression") {
	      var right = n.asInstanceOf[BinaryExpression].get_right;
	      var rt = right.get_type;
	      println("8888888888888888888888888888888888888888888888 > " + rt);
	      if (rt == "number"||rt == "string"||rt == "character"||rt == "true"||rt == "false") {
	        var left = n.asInstanceOf[BinaryExpression].get_left;    //left should be now a variable or error.
	        var str = left.asInstanceOf[VariableExpression].get_variable_name;
	        var t = left.asInstanceOf[VariableExpression].get_variable_type;
	        var llvm_type = llvm_types(t);
	        var llvm_line: String = "";
	        var value = "";
	        if (left.get_type == "variable" && rt == "string") {
	          value = get_string_value(right.asInstanceOf[StringExpression]);
	          llvm_type = get_string_type(right.asInstanceOf[StringExpression]);
	          var back_ref = get_back_refrence_for_string(right.asInstanceOf[StringExpression], prefix, str);
	          globals_map += (str -> back_ref);
	          llvm_line = prefix + str + " = " + "private unnamed_addr constant " + llvm_type + value;
	          println(back_ref);
	        }else if (left.get_type == "variable") {
	          llvm_type = llvm_types(t);
	          var back_ref = llvm_type + "* " + prefix  + str;
	          rt match {
	            case "true" => value = "1";
	            case "character" => value = right.asInstanceOf[CharacterExpression].get_value;
	            case "number" => value = get_number_value_str(right.asInstanceOf[NumberExpression]);
	            case _ => value = "0";
	          }
	          globals_map += (str -> back_ref);
	          llvm_line = prefix + str + " = " + "private unnamed_addr constant " + llvm_type + " " + value;
	        }
	        writer.writeln(llvm_line);
	      }
	    }
	  }
	  
	  def set_global_types(x: MultBody) {
	    val globals = x.get_body;
	    for (n <- globals if n.get_type == "binary_expression") {
	      var right = n.asInstanceOf[BinaryExpression].get_right;
	      var rt = right.get_type;
	      if (rt == "number"||rt == "string"||rt == "character"||rt == "true"||rt == "false") {
	        var left = n.asInstanceOf[BinaryExpression].get_left;  //left should be now a variable or error.
	        rt match {
	          case "true"|"false" => left.asInstanceOf[VariableExpression].set_variable_type("boolean");
	          case "character"|"string" => left.asInstanceOf[VariableExpression].set_variable_type(rt);
	          case "number" => left.asInstanceOf[VariableExpression].set_variable_type(right.asInstanceOf[NumberExpression].get_number_type);
	          case _ => println("Ufo Kidnapped me. ");
	        }
	      }
	    }
	    for (n <- globals if n.get_type == "binary_expression") {
	      var right = n.asInstanceOf[BinaryExpression].get_right;
	      var rt = right.get_type;
	      if (rt == "variable") {
	        var left = n.asInstanceOf[BinaryExpression].get_left;  //left should be now a variable or error.
	        var t = right.asInstanceOf[VariableExpression].get_variable_type;
	        left.asInstanceOf[VariableExpression].set_variable_type(t);
	        if (t == "") {
	          println("ERROR: Type uknown. " + left.get_type + ": " + left.asInstanceOf[VariableExpression].get_variable_name);
	        }
	      }
	    }
	  }
	  
	  def free_appendix(app: Int) {
	    AppendixProducer.free_appendix(app);
	  }
	  
	  def get_appendix(): Int = {
	    return AppendixProducer.get_ref();
	  }
	}
}