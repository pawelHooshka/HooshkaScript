package scanner.ast {

import java.util.ArrayList;

abstract class Expression() {
  
  var expr_type = ""; 
  var temp_index_in_line: Int = -1;
  var parent: Expression = null;
  var ref_expr = RefProducer.get_ref();
  
  def get_ref(): Int = {
    return ref_expr;
  }
  
  def set_ref(r: Int) {
    ref_expr = r;
  }
  
  def free_ref() {
    //This procedure is supposed to be used almost as destructor. That is - only when expression node is definitely to be destroyed.
    RefProducer.free_ref(ref_expr);
  }
  
  def is_opennable(): Boolean = {
    return false;
  }
  
  def has_multi_body(): Boolean = {
    return false;
  }
  
  def get_line_index(): Int = {
    return temp_index_in_line;
  }
  
  def set_line_index(x: Int) {
    temp_index_in_line = x;
  }
  
  def set_type(expression_type: String){
   expr_type = expression_type;
  }
  
  def get_type(): String = {
    return expr_type;
  }
  
  def get_parent(): Expression = {
    return parent;
  }
  
  def set_parent(p: Expression) {
    parent = p;
  }
}

class TempExpression extends Expression {
  
  expr_type = "temporary";
  var tokens = List[Expression]();
  
  def get_tokens(): List[Expression] = {
    return tokens;
  }
  
  def set_tokens(ts: List[Expression]) {
    tokens = ts;
  }
}

class TrueExpression extends Expression {
  
  expr_type = "true";
}

class FalseExpression extends Expression {
  
  expr_type = "false";
}

//The below classes will be discarded upon reading. Only to know the indentation level.
abstract class Brackets extends Expression {
  
  expr_type = "bracket";
  var enclosed_exprs = List[Expression]();
  var bracket = "";
  var level = 0;
  var open: Boolean = true;
  
  def add_to_enclosed(ex: Expression) {
    enclosed_exprs ::= ex;
  }
  
  def get_all_enclosed(): List[Expression] = {
    return enclosed_exprs;
  }
  
  def set_bracket(bracket_type: String) {
    bracket = bracket_type;
  }
  
  def get_bracket(): String = {
    return bracket;
  }
  
  def set_level(levl: Int) {
    level = levl;
  }
  
  def get_level(): Int = {
    return level;
  }
  
  def set_open() {
    open = true;
  }
  
  def set_close() {
    open = false;
  }
  
  def get_open_state(): Boolean = {
    return open;
  }
}

class OpenRoundBracket extends Brackets with SingleBody {
  
  set_bracket("(");
  open = true;
}

class ClosedRoundBracket extends Brackets {
  
  set_bracket(")");
  open = false;
}

class OpenCurlyBracket extends Brackets {
  
  set_bracket("{");
  open = true;
}

class ClosedCurlyBracket extends Brackets {
  
  set_bracket("}");
  open = false;
}

class OpenSquareBracket extends Brackets with SingleBody {
  
  set_bracket("[");
  open = true;
}

class ClosedSquareBracket extends Brackets {
  
  set_bracket("]");
  open = false;
}

abstract class NormalExpression extends Expression {
  
}

abstract class KeywordExpression extends Expression {
  
}

class YieldExpression extends KeywordExpression with MultBody {
  
  expr_type = "yield";
  
  override def has_multi_body(): Boolean = {
    return multi;
  }
}

class ReturnExpression extends KeywordExpression with MultBody {
  
  expr_type = "return";
  
  override def has_multi_body(): Boolean = {
    return multi;
  }
}

class RepeatExpression extends KeywordExpression {
  
  expr_type = "repeat";
}

class BreakExpression extends KeywordExpression {
  
  expr_type = "break";
}

class CharacterExpression(value: String) extends NormalExpression {
  
  expr_type = "character";
  var char = value;
  
  def get_value(): String = {
    return char;
  }
  
  def set_value(v: String) {
    char = v;
  }
}

class StringExpression(value:  String) extends CharacterExpression(value) {
  
  expr_type = "string";

}

class NumberExpression(namex: String, num_type: String, var int_number: Int, var dec_number: Int) extends NormalExpression with Signable { 
  
  expr_type = "number";
  var number_type = num_type;   //It could be: int, real
  var name = namex;
  var integer_part = int_number;
  var decimal_part = dec_number;
  
  def set_integer_part(i: Int) {
    integer_part = i;
  }
  
  def set_decimal_part(d: Int) {
    decimal_part = d;
  }
  
  def get_number_type(): String = {
    return number_type;
  }
  
  def set_number_type(t: String) {
    number_type = t;
  }
  
  def get_name(): String = {
    return name;
  }
  
  def get_integer_part(): Int = {
    return integer_part; 
  }
  
  def get_decimal_part(): Int = {
    return decimal_part;
  }
}

class BinaryExpression(operator: String, l: NormalExpression, r: NormalExpression) extends NormalExpression with SingleBody {
  
  expr_type = "binary_expression";
  var binary_operator = operator;    //It could be: >< * / + - ^ % = :- this last two are assignments. Especially last one.
  left = l;
  right = r;
  
  var multiple_left = List[Expression]();
  
  def get_multiple_left(): List[Expression] = {
    return multiple_left.reverse;
  }
  
  def set_multiple_left(mult_left: List[Expression]) {
    multiple_left = mult_left.reverse;
  }
  
  def add_to_left(e: Expression) {
    multiple_left ::= e;
  }
  
  def get_bin_op(): String = {
    return binary_operator;
  }
  
  def set_bin_op(op: String) {
    binary_operator = op;
  }
  
  def get_operation(): String = {
    return binary_operator;
  }
}

class ConditionOperatorExpression(conditional_operator: String, l: NormalExpression, r: NormalExpression) extends NormalExpression with SingleBody with Booleanable {
  
  operator = conditional_operator;
  expr_type = "conditional_expression";
  left = l;
  right = r;
}

class RelationalExpression(rel_operator: String, l: NormalExpression, r: NormalExpression) extends NormalExpression with SingleBody with Booleanable {
  
  operator = rel_operator;
  expr_type = "relational_expression";
  left = l;
  right = r;
}

class VariableExpression(namex: String, size: Int) extends NormalExpression with Signable {
  
  expr_type = "variable";
  var variable_type = "";
  var variable_value: Expression = null;
  
  var name = namex;
  var variable_size = size;
  
  def set_value(e: Expression) {
    variable_value = e;
  }
  
  def get_value(): Expression = {
    return variable_value;
  }
  
  def set_variable_type(v_type: String) {
    variable_type = v_type;
  }
  
  def get_variable_type(): String = {
    return variable_type;
  }
  
  def get_variable_name(): String = {
    return name;
  }
  
  def get_variable_size(): Int = {
    return variable_size;
  }
}

class FlowControlExpression extends NormalExpression with MultBody with Opennable {
  
  override def has_multi_body(): Boolean = {
    return multi;
  }
  
  override def is_opennable(): Boolean = {
    return true;
  }
}

trait Booleanable {
  
  var operator: String = null;
  
  def set_operator(op: String) {
    operator = op;
  }
  
  def get_operator(): String = {
    return operator;
  }
}

trait SingleBody {
  
  var left: Expression = null;
  var right: Expression = null;
  
  def get_left(): Expression = {
    return left;
  }
  
  def get_right(): Expression = {
    return right;
  }
  
  def set_left(l: Expression) {
    left = l;
  }
  
  def set_right(r: Expression) {
    right = r;
  }
}

trait MultBody {
  
  var multi = true;
  var body = List[Expression]();
  var mult = "multi";
  
  def get_body_size(): Int = {
    return body.length;
  }
  
  def set_body(bs: List[Expression]) {
    body = bs;
  }
  
  def add_to_body(exp: Expression) {
    if (body == null) {
      body = List[Expression]();
    }
    body ::= exp;
  }
  
  def get_body(): List[Expression] = {
    if (body != null) {
      return body.reverse;
    }else{
      return null;
    }
  }
  
  def has_multi_body(): Boolean = {
    return multi;
  }
  
  def get_multi_state(): String = {
    return mult;
  }
}

trait Opennable {
  
  var openned = false;
  
  def set_openned(b: Boolean) {
    openned = b;
  }
  
  def get_opened(): Boolean = {
    return openned;
  }
  
  def is_opennable(): Boolean = {
    return true;
  }
}

trait Signable {
  
  var sign: String = null;
  
  def update_sign() {
    if (sign != null && sign == "-") {
      sign = null;
    }else{
      sign = "-";
    }
  }
  
  def get_sign(): String = {
    return sign;
  }
  
  def set_sign(s: String) {
    sign = s;
  }
} 

class WhileExpression(var body_branch: List[Expression], var condition_branch: Booleanable) extends FlowControlExpression {
  
  expr_type = "while_loop";
  body = body_branch;
  
  def set_condition(c: Booleanable) {
    condition_branch = c;
  }
  
  def get_condition(): Booleanable = {
    return condition_branch;
  }
}


class LoopExpression(body_right_branch: List[Expression]) extends FlowControlExpression {
  
  expr_type = "loop";
  body = body_right_branch;
}

class LoopForExpression(left_over_branch: IteratorCallingExpression, middle_return_vars: List[Expression], body_right_branch: List[Expression]) extends FlowControlExpression {
  
  expr_type = "loop_for_over";
  var left_iterator_branch = left_over_branch;
  var iterator_return_vars = middle_return_vars;
  body = body_right_branch;
  
  def set_iter(iter: IteratorCallingExpression) {
    left_iterator_branch = iter;
  }
  
  def get_iterator(): IteratorCallingExpression = {
    return left_iterator_branch;
  }
  
  def get_loop_condition(): IteratorCallingExpression = {
    return left_iterator_branch;
  }
  
  def set_iterator_vars(iter_vars: List[Expression]) {
    iterator_return_vars = iter_vars;
  }
  
  def add_iter_var(v: VariableExpression) {
    if(iterator_return_vars == null){
      iterator_return_vars = List[VariableExpression]();
    }
    iterator_return_vars ::= v;
  }
  
  def get_iterator_vars(): List[Expression] = {
    return iterator_return_vars.reverse;
  }
}

class IfExpression(condition: Expression, b: List[Expression]) extends FlowControlExpression {
  
  expr_type = "if";
  var condition_left_branch = condition;
  body = b;
  var else_or_elif_branch: FlowControlExpression = null;
  
  def set_condition(c: Expression) {
    condition_left_branch = c;
  }
  
  def get_condition(): Expression = {
    return condition_left_branch;
  }
  
  def set_else_or_elif_branch(branch: FlowControlExpression) {
    else_or_elif_branch = branch;
  }
  
  def get_else_elif_branch(): FlowControlExpression = {
    return else_or_elif_branch;
  }
}

class ElifExpression(condition: Expression, b: List[Expression], optional_else_or_elif: Expression) extends FlowControlExpression {
  
  expr_type = "elif";                        //This expression can exist only inside if expression.
  var condition_left_branch = condition;
  body = b;

  def set_condition(c: Expression) {
    condition_left_branch = c;
  }
  
  def get_condition(): Expression = {
    return condition_left_branch;
  }
}

class ElseExpression(body: List[Expression]) extends FlowControlExpression {
  
  expr_type = "else"; //This expression can exist only inside if expression.
  var body_branch_right = body;
}


class Definition(bd: List[Expression], parameters: List[NormalExpression]) extends NormalExpression with MultBody with Opennable {
  
  body = bd;
  var pi_parameters: List[Expression] = null;
  
  def set_params(params: List[Expression]) {
    pi_parameters = params.reverse;
  }
  
  def add_param(param: NormalExpression) {
    if (pi_parameters == null) {
      pi_parameters = List[NormalExpression]();
    }
    pi_parameters ::= param;
  }
  
  def get_parameters(): List[Expression] = {
    if (pi_parameters != null) {
      return pi_parameters.reverse;
    }else{
      return null;
    }
  }
  
  override def has_multi_body(): Boolean = {
    return multi;
  }
  
  override def is_opennable(): Boolean = {
    return true;
  }
}


class ProcedureDefinitionExpression(bd: List[Expression], parameters: List[NormalExpression]) extends Definition(bd, parameters) {
  
  expr_type = "procedure_definition";
  body = bd;
  pi_parameters = parameters;
}

class ProcedureCallingExpression(callerx: Expression, arguments: List[Expression]) extends NormalExpression with Signable{
  
  expr_type = "procedure_call";
  
  var caller = callerx;
  var called_procedure_arguments = arguments;
  
  def set_caller(caller_node: Expression) {
    caller = caller_node;
  }
  
  def get_caller(): Expression = {
    return caller;
  }
  
  def get_arguments(): List[Expression] = {
    return called_procedure_arguments.reverse;
  }
  
  def set_arguments(args: List[Expression]) {
    called_procedure_arguments = args;
  }
  
  def add_argument(arg: NormalExpression) {
    if (called_procedure_arguments == null) {
      called_procedure_arguments = List[NormalExpression]();
    }
    called_procedure_arguments ::= arg;
  }
}

class IteratorDefinitionExpression(bd: List[Expression], parameters: List[NormalExpression]) extends Definition(bd, parameters) {
  
  expr_type = "iterator_definition";
  pi_parameters = parameters;
  body = bd;
}

class IteratorCallingExpression(arguments: List[Expression]) extends NormalExpression with Signable {
  
  expr_type = "iterator_call";
  var called_iter_arguments = arguments;
  
  def set_arguments(args: List[Expression]) {
    called_iter_arguments = args;
  }
  
  def add_argument(arg: NormalExpression) {
    if (called_iter_arguments == null) {
      called_iter_arguments = List[NormalExpression]();
    }
    called_iter_arguments ::= arg;
  }
  
  def get_called_iter_arguments(): List[Expression] = {
    return called_iter_arguments.reverse;
  }
}

class GlobalExpression extends Expression with MultBody with Opennable {
  
  expr_type = "global";
  
  override def has_multi_body(): Boolean = {
    return multi;
  }
  
  override def is_opennable(): Boolean = {
    return true;
  }
}

class ExtendsExpression(left_class: ClassExpression, right_class: ClassExpression) extends KeywordExpression {
  
  expr_type = "extends";
  var left_class_extended = left_class;
  var right_class_extention = right_class;
  
  def get_extended_class(): ClassExpression = {
    return left_class_extended;
  }
  
  def get_right_class_extension(): ClassExpression = {
    return right_class_extention;
  }
}

class ClassExpression(name: String) extends NormalExpression with MultBody with Opennable {
  
  expr_type = "class";
  var class_name = name;
  //var extend_block: ExtendsExpression = null;
  
  def set_class_name(n: String) {
    class_name = n;
  }
  
  def get_class_name(): String = {
    return class_name;
  }
  
  override def has_multi_body(): Boolean = {
    return multi;
  }
  
  override def is_opennable(): Boolean = {
    return true;
  }
}
}