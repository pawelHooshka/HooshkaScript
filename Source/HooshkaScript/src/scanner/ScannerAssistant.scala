package scanner {

import scala.collection.mutable.HashMap
import scanner.ast.Expression
import scala.util.matching.Regex

class ScannerAssistant {
  
  val map = HashMap[Int, List[String]]();
  
  val OPERATOR = 0;
  val CONDITION_TEST = 1;
  val BRACKET_OP = 2;
  val BRACKET_CL = 3;
  val STATEMENT_BRACKET_OP = 4;
  val STATEMENT_BRACKET_CL = 5;
  val LOGIC = 6;
  val KEYWORD =7;
  val TYPE_DECLARATION = 8;
  val PRIMITIVE = 9;
  
  val assignment_separator = " ";
  val normal_separator = ",";

  val operators = List("%", "^", "+", "-", "*", "/", "=", ":-");
  val condition_tests = List("==", ":-", ">", "<", "><", "<=", ">=", "~=", "~:-");
  val bracket_open = List("(");
  val bracket_close = List(")");
  val statement_bracket_open = List("{");
  val statement_bracket_close = List("}")
  val logic_operators = List("~", "&&", "||", "|", "&");
  val keywords = List("<--", "=>", "proc", "iter", "loop", "for", "loop for", "over ", "if", "then", "else", "elif", "match", "case", ":", "break", "repeat");
  val primiteves = List("int", "real", "bool", "char", "object");
  
  var current_in_bracket_level = 0;
  var current_in_curl_bracket_level = 0;
  var current_in_sq_bracket_level = 0;
  var last_unescaped_quote_apostrophe: Char = 0;
  var not_escaped_quote_apostrophe = 0;
  
  val signs = List[String]("\"", "\'", ",", " ", "~", "&&", "||", "|", "&", "==", ":-", ">", "<", "><", "<=", ">=", "~=", "%", "^", "+", "-", "*", "/", "=", ":-");
  val special_signs = List[String](":-", "*", "=", "==", "-", "+", "/", "&&", "||", "<", ">", ">=", "<=", "^", "%");
  val special_chars = List[Char]('|', '-', '+', '=', ':', '>', '<', '/', '&', '^', '%', '*');
 
  def is_a_sign(elem: String): Boolean = {
    if (signs.contains(elem)) {
      return true;
    }
    return false;
  }
  
  def escaped(elem: Char, str: String, current_index: Int): Boolean = {
    if (current_index == 0) {
      return false;
    }
    var char: Char =  str.charAt(current_index - 1);
    if (char == '\\') {
      return true;
    }
    return false;
  }
  
  def is_type(ref: Any): String = {
    ref match {
      case x: String => return "String";
      case y: Int => return "Int";
      case z: Char => return "Char";
      case _ => return "Uknown";
    }
  } 
  
  //procedure ignores " and '. That is anything in between those symbols is not split.
  def split_str_on_char(str: String, split: Char): List[String] = {
    println()
   // println("LINE IS: " + str)
    println()
    var unescaped_quotes = 0;
    var unescaped_apostrophes = 0;
    var strs = List[String]();
    var curr_str = "";
    var count = 0;
    for (char <- str) {
      if (((char == split || char.toString == '\n') && curr_str.length > 0) || (char == '\"' || char == '\'') && !escaped(char, str, count)) {
        if (unescaped_quotes == 2) {
          curr_str = curr_str + char;
        }
        strs ::= curr_str;
        curr_str = "";
        if (unescaped_quotes > 1) {
          unescaped_quotes = 0;
        }
        if (unescaped_apostrophes > 1) {
          unescaped_apostrophes = 0;
        }
        if (char == '\"' && !escaped(char, str, count)) {
          unescaped_quotes = unescaped_quotes + 1;
          curr_str = curr_str + char;
        }else if (char == '\'' && !escaped(char, str, count) && unescaped_quotes == 0) {
          unescaped_apostrophes = unescaped_apostrophes + 1;
          curr_str = curr_str + char;
        }
      }else{
        curr_str = curr_str + char;
      }
      count = count + 1;
    }
    return strs.reverse;
  }
  
  
  
  
    def breakout_aposed(s: String): List[String] = {
    var strings: List[String] = List[String]();
    var curr_s: String = "";
    var start = false;
    var stop = false;
    var in = false;
    var ind = 0;
    var len = s.length() - 1;
    for (i <- 0 to len) {
      var c = s.charAt(i);
      curr_s = curr_s + c;
      //println(">: " + curr_s);
      if (c == '\'' && !escaped(c, s, ind) && !in) {
        start = true;
        stop = false;
      }
      if (in && c == '\'' && !escaped(c, s, ind)) {
        stop = true;
        in = false;
      }
      if (stop && c == '\'' && !escaped(c, s, ind)) {
        strings ::= curr_s;
        curr_s = "";
      }
      if (start) {
        //println("Now curr_s is: " + curr_s);
        var ch = curr_s.charAt((curr_s.length() - 1));  
        curr_s = curr_s.substring(0, (curr_s.length() - 1));
        strings ::= curr_s;
        curr_s = ch.toString();
        start = false;
        in = true;
      }
      ind = ind + 1;
     // if (i == len) {
        //println("----------------------------______>");
      //  strings ::= curr_s;
      //}
    }
    if (strings.length == 0) {
      return List[String](s);
    }
    return strings.reverse;
  }
  
  
  
  
  def breakout_quoted(s: String): List[String] = {
    var strings: List[String] = List[String]();
    var curr_s: String = "";
    var start = false;
    var stop = false;
    var in = false;
    var ind = 0;
    var len = s.length() - 1;
    for (i <- 0 to len) {
      var c = s.charAt(i);
      curr_s = curr_s + c;
      //println(">: " + curr_s);
      if (c == '\"' && !escaped(c, s, ind) && !in) {
        start = true;
        stop = false;
      }
      if (in && c == '\"' && !escaped(c, s, ind)) {
        stop = true;
        in = false;
      }
      if (stop && c == '\"' && !escaped(c, s, ind)) {
        strings ::= curr_s;
        curr_s = "";
      }
      if (start) {
        //println("Now curr_s is: " + curr_s);
        var ch = curr_s.charAt((curr_s.length() - 1));  
        curr_s = curr_s.substring(0, (curr_s.length() - 1));
        strings ::= curr_s;
        curr_s = ch.toString();
        start = false;
        in = true;
      }
      ind = ind + 1;
     // if (i == len) {
        //println("----------------------------______>");
      //  strings ::= curr_s;
      //}
    }
    if (strings.length == 0) {
      return List[String](s);
    }
    return strings.reverse;
  }

  
  def split_into_token_strings(strs: List[String]): List[String] = {
    //println("___________>>>: " + strs);
    var strings = List[String]();
    for (str <- strs) {
      //println("++++++++++++++++++: " + str);
      var s = str.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
      if (s.length() > 0 && (s.charAt(0) == '\"' || s.charAt(0) == '\'')) {
       // var s = str.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
        strings ::= s;
      }else{
        var subs = split_on_space(str);
        for (s_s <- subs) {
          strings ::= s_s;
        }
      }
    }
    return strings.reverse;
  }
  
  
  def split_on_space(str: String): List[String] = {
    var s = str.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
    var strings = List[String]();
    var curr_s = "";
    var len = (s.length() - 1);
    if (len == 0) {
      return List[String](s);
    }
    var last_append_i = 0;
    for (i <- 0 to len) {
      var c = s.charAt(i);
      if (c == ' ') {
        if (curr_s != "") {
          strings ::= curr_s;
          last_append_i = i;
        }
        curr_s = "";
      }else{
        curr_s = curr_s + c;
      }
    }
    if (last_append_i < len) {
      strings ::= s.substring(last_append_i + 1);
    }
    return strings.reverse;
  }
  
  
  """
  def update_before_round_brackets(strs: List[String], variable_regex: Regex): List[String] = {
    var str_new1 = ""
    var strings1 = List[String]();
    for (str <- strs) {
      if (str.charAt(0) == "(") {
        var sub_a = str.substring(0,1);
        var pre_sub_b = str.substring(1);
        var sub_b = pre_sub_b.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
        strings1 ::= sub_a;
        if (sub_b.length() > 0) {
          strings1 ::= sub_b;
        }
      }
    }
    strings1 = strings1.reverse;
    var add_old = true;
    val regex = variable_regex;
    var strings = List[String]();
    var op_allowed = false;
    var current_str_index_in_list = 0;
    for (str <- strings1) {
      var str_new = "";
      str match {
        case regex() => op_allowed = true;
        case _ => op_allowed = false;
      }
      if (op_allowed && (current_str_index_in_list < (str.length - 1))) {
        if (strings1(current_str_index_in_list + 1) == "(") {
          str_new = str + "(";
          strings ::= str_new;
          add_old = false;
        }
      }else{
        if (add_old) {
          strings ::= str;
        }else{
          add_old = true;
        }
      }
      current_str_index_in_list = current_str_index_in_list + 1;
    }
    return strings.reverse;
  }
  
  
  def update_brackets_in_strings(strs: List[String]): List[String] = {
    var strings = List[String]();
    for (str <- strs) {
      var str_new = "";
      for (char <- str) {
        if (char == "(" || char == "[" || char == "{") {
          str_new = str_new + (char + " ");
        }else if (char == ")" || char == "]" || char == "}") {
          str_new = str_new + (" " + char);
        }else{
          str_new = str_new + char;
        }
      }
      if (str_new.length > 0) {
        strings ::= str_new;
      }
    }
    if (strings.length > 0) {
      return strings.reverse;
    }
    return strings;
  }
  """
  def update_before_brackets(str: String): String = {
    var string_new = "";
    var len = str.length();
    var u_quote = false;
    var u_apos = false;
    for (index <- 0 to (len - 1)) {
      var char = str.charAt(index);
      if ((char == '\"') && !escaped(char, str, index)) {
        u_quote = !u_quote;
      }
      if ((char == '\'') && !escaped(char, str, index)) {
        u_apos = !u_apos;
      }
      if ((char == '(' || char == '{' || char == ')' || char == '}' || char == ']') && (!u_quote && !u_apos) && ((index > 0) && str.charAt(index - 1) != ' ')) {
        string_new = string_new + ' ';
      }
      string_new = string_new + char;
    }
    return string_new;
  }
  
  
  
  def update_after_brackets(str: String): String = {
    var string_new = "";
    var len = str.length();
    var u_quote = false;
    var u_apos = false;
    for (index <- 0 to (len - 1)) {
      var char = str.charAt(index);
      string_new = string_new + char;
      if ((char == '\"') && !escaped(char, str, index)) {
        u_quote = !u_quote;
      }
      if ((char == '\'') && !escaped(char, str, index)) {
        u_apos = !u_apos;
      }
      if ((char == '(' || char == '{' || char == '}' || char == '[') && (!u_quote && !u_apos) && (index < (len - 1) && str.charAt(index + 1) != ' ')) {
        string_new = string_new + ' ';
      }
    }
    return string_new;
  }
  
  
  def add_space_after_and_before_spcial_signs(s: String): String = {
    var str_new = "";
    var curr_s = "";
    var start = false;
    var after = false;
    var in = false;
    for (c <- s) {
      if (special_chars.contains(c) && !in) {
        str_new = str_new + ' ';
        in = true;
      }
      if (!special_chars.contains(c)) {
        if (in) {
          //after = true;
          str_new = str_new + ' ';
        }
        in = false;
      }
      str_new = str_new + c;
    }
    return str_new;
  }
  
  
  
  def update_loops(tokensx: List[String]): List[String] = {
    var tokens = tokensx.reverse;
    var tokens_new = List[String]();
    var len = tokens.length - 1;
    var last_t = "";
    var count = 0;
    for (t <- tokens) {
      if (t != "loop" && t != "for") {
        if (last_t == t) {
          println("ERROR - syntax error: " + t);
        }
        tokens_new ::= t;
        last_t = t;
      }else if (t == "loop" && count < len && tokens(count + 1) == "for") {
        tokens_new ::= "loop for";
      }else if (t == "loop" && count < len && tokens(count + 1) != "for") {
        if (last_t == t) {
          println("ERROR - syntax error: " + t);
        }
        tokens_new ::= t;
      }
      count = count + 1;
    }
    return tokens_new.reverse;
  }
  
  
  
  def process_line(linex: String, counter: Int, variable_regex: Regex): List[String] = {
    var countr = 0;
    if (linex == null) {
      return null;
    }
    var pre_tokens = List[String]();
    var line = linex.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
    line = line.dropWhile(_ == '\t').reverse.dropWhile(_ == '\t').reverse;
    val line_with_signs = add_space_after_and_before_spcial_signs(line);
    //println("Line with special signs updated: " + line_with_signs);
    val line_bracketed = update_before_brackets(line_with_signs);
    //println("111111111111111111111111");
    //println("Linea: " + line_bracketed);
    val line_bracketed2 = update_after_brackets(line_bracketed);
    //println("222222222222222222222222");
    //println("Lineb: " + line_bracketed2);
    var pre_pre_tokens = breakout_quoted(line_bracketed2);
    //println("pre_pre_pre_pre_pre: " + pre_pre_tokens);
    var pre_tokens_aposed = List[String]();
    for (str <- pre_pre_tokens) {
      if (str.length() > 0 && str.charAt(0) != '"') {
        var pre_tokens_list = breakout_aposed(str); 
        for (pt <- pre_tokens_list) {
          pre_tokens_aposed ::= pt;
        }
      }else{
        pre_tokens_aposed ::= str;
      }
    }
    println(">>>>>>>>>>>>> " + pre_tokens_aposed.reverse);
    //println("333333333333333333333333");
    //println("pre_pre_pre_pre_pre: " + pre_pre_tokens);
    pre_tokens = split_into_token_strings(pre_tokens_aposed.reverse);
    //println("444444444444444444444444");
    //pre_tokens = update_brackets_in_strings(pre_pre_tokens);
    println("PRE_TOKENS ARE: " + pre_tokens);
    var pre_tokens2 = List[String]();
    for (pre_token <- pre_tokens) {
      var pre_token2 = pre_token.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
      //pre_token2 = pre_token.dropWhile(_ == '\t').reverse.dropWhile(_ == '\t').reverse;
      pre_tokens2 ::= pre_token2;
    }
    pre_tokens2 = update_loops(pre_tokens2);
    //pre_tokens2 = pre_tokens2.dropWhile(_ == ',').reverse.dropWhile(_ == ',').reverse;
    //pre_tokens2 = pre_tokens2.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
    //pre_tokens2.foreach(_.dropWhile(_ == ',').reverse.dropWhile(_ == ',').reverse);
    //pre_tokens2.foreach(_.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse);
    var pre_tokens3 = List[String]();
    for (t <- pre_tokens2) {
      var pre_token3 = t.dropWhile(_ == ',').reverse.dropWhile(_ == ',').reverse;
      pre_tokens3 ::= pre_token3;
    }
    pre_tokens2 = pre_tokens3.reverse;
    println("PRE_TOKENS 2222 ARE: " + pre_tokens2);
    return pre_tokens2;
  }
   
  
  def process_line2(linex: String, counter: Int): List[String] = {
    var countr = 0;
    if (linex == null) {
      return null;
    }
    val line = linex.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
    println("line: " + line);
    var go = 0;
    var pre_tokens = List[String]();
    var sign =  false;
    var last_token = "";
    for(i <- 0 to (line.length() - 1)){
      var char = line.charAt(i);
      var str = char.toString();
      println("STR: " + str);
      sign = is_a_sign(str);
      if ((sign || i == line.length() - 1) && go == 1) {
        pre_tokens ::= last_token;
        last_token = "";
        if (str != "\""  && str != "\'" && str != " ") {
          pre_tokens ::= str;
        }
        go = 0;
      }else{
        last_token = last_token + char;
        go = 1;
      }
      if (not_escaped_quote_apostrophe == 0 && (str == "\"" || str == "\'") && !escaped(char, line, countr)) {
        not_escaped_quote_apostrophe = not_escaped_quote_apostrophe + 1;
        last_token = str;
      }else if (not_escaped_quote_apostrophe == 1 && (str == "\"" || str == "\'") && !escaped(char, line, countr)) {
        not_escaped_quote_apostrophe = 0;
        pre_tokens = pre_tokens.map(s => if (pre_tokens.indexOf(s) == 1) s + str else s);
      }
      countr = countr + 1;
      last_token = last_token.dropWhile(_ == ' ').reverse.dropWhile(_ == ' ').reverse;
    }//temporarily NO MULTIPLE ASSIGNEMENTS ARE ALLOWED.
    map.put(counter, pre_tokens);
    println(pre_tokens);
    return pre_tokens.reverse;
  }
  
  def is_in_brackets(ch: Char): Boolean = {
    if(ch == '('){
      current_in_bracket_level = current_in_bracket_level + 1;
    }
    if(ch == ')'){
      current_in_bracket_level = current_in_bracket_level - 1;
    }
    if(current_in_bracket_level > 0){
      return true;
    }else if(current_in_bracket_level < 0){
      Console.err.println("Closing bracket ", ")", " does not macth an openning one. ");
      return false;
    }else{
      return false;
    }
  }
  
  def is_in_curl_brackets(ch: Char): Boolean = {
    if(ch == '{'){
      current_in_curl_bracket_level = current_in_curl_bracket_level + 1;
    }
    if(ch == '}'){
      current_in_curl_bracket_level = current_in_curl_bracket_level - 1;
    }
    if(current_in_curl_bracket_level > 0){
      return true;
    }else if(current_in_curl_bracket_level < 0){
      Console.err.println("Closing curly bracket ", "}", " does not macth an openning one. ");
      return false;
    }else{
      return false;
    }
  }
    
  def is_in_square_brackets(ch: Char): Boolean = {
    if(ch == '['){
      current_in_sq_bracket_level = current_in_sq_bracket_level + 1;
    }
    if(ch == ']'){
      current_in_sq_bracket_level = current_in_sq_bracket_level - 1;
    }
    if(current_in_sq_bracket_level > 0){
      return true;
    }else if(current_in_sq_bracket_level < 0){
      Console.err.println("Closing bracket ", "]", "does not macth an openning one. ");
      return false;
    }else{
      return false;
    }
  } 
  
  def list_has(list: List[String], element: String): Boolean = {
    if (list != null && !list.isEmpty){
      return list.contains(element);
    }
    return false;
  }
}
}