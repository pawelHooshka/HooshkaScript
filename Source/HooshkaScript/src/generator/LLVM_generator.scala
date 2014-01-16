package generator {

  import parser.Parser
  import scanner.ast.Expression
  import io.FileWriter;
  import scanner.ast.MultBody

	class LLVM_generator(parserx: Parser, out_path: String) extends LLVM_coder {
		
	  val parser = parserx;
	  val writer = new FileWriter(out_path);
	  
	  /*def emit_SSA(n: Expression): String = {
	    var ssa = "";
	    val t = n.get_type;
	    t match {
	      case "true"|"false" => ssa = emit_ssa_boolean_literal();
	      case "bracket" => ssa = emit_ssa_bracket();
	      case "yield" => ssa = emit_ssa_yield();
	      case "return" => ssa = emit_ssa_return();
	      case "repeat" => ssa = emit_ssa_repeat();
	      case "break" => ssa = emit_ssa_break();
	      case "character" => ssa = emit_ssa_character();
	      case "string" => ssa = emit_ssa_string();
	      case "number" => ssa = emit_ssa_number();
	      case "binary_expression" => ssa = emit_ssa_binary_expr();
	      case "conditional_expression" => ssa = emit_ssa_conditional_expr();
	      case "relational_expression" => ssa = emit_ssa_relational_expr();
	      case "variable" => ssa = emit_ssa_variable();
	      case "loop" => ssa = emit_ssa_loop();
	      case "loop_for_over" => ssa = emit_ssa_lfo();
	      case "if" => ssa = emit_ssa_if();
	      case "elif" => ssa = emit_ssa_elif();
	      case "else" => ssa = emit_ssa_else();
	      case "procedure_definition" => ssa = emit_ssa_procedure_def();
	      case "procedure_call" => ssa = emit_ssa_procedure_call();
	      case "iterator_definition" => ssa = emit_ssa_iterator_def();
	      case "iterator_call" => ssa = emit_ssa_iterator_call();
	      case "global" => ssa = emit_ssa_global();
	      case "extends" => ssa = emit_ssa_extends();
	      case "class" => ssa = emit_ssa_class();
	      case _ =>
	    }
	    return ssa;
	  }*/
	  
	  
	  def compile() {
	    val rootAST = parser.get_AST_root();
	    set_global_types(rootAST.asInstanceOf[MultBody]);
	    set_globals(rootAST.asInstanceOf[MultBody], writer);
	  }
	}
}