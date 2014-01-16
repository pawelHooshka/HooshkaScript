package generator {

	import scala.collection.mutable.Stack

	object AppendixProducer {
	  
	  var ref = 0;
	  
	  var empties = Stack[Int]();
	  
	  def free_appendix(appendix: Int) {
	    //Only to be used when some node is definitely going to be deleted.
	    empties.push(appendix);
	  }
	  
	  def get_ref(): Int = {
	    if (!empties.isEmpty) {
	      return empties.pop;
	    }
	    ref = ref + 1;
	    return ref;
	  }
	}
}