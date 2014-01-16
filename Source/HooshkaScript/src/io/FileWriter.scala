package io {

	import java.io.File
	import java.io.PrintWriter

	class FileWriter(path: String) {
	  
	  val file = new File(path);
	  
	  val writer = new PrintWriter(file);
	  
	  def writeln(x: Any) {
	    x match {
	      case x:String => writer.println(x.asInstanceOf[String]); println(x.asInstanceOf[String]);
	      case _ =>	writer.println(x.toString); println(x.toString);
	    }
	    writer.flush();
	  }
	  
	  def write(x: String) {
	    x match {
	      case x:String => writer.print(x.asInstanceOf[String]);
	      case _ =>	writer.print(x.toString);
	    }
	  }
	  
	  def close() {
	    writer.close();
	  }
	}
}