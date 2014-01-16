package scanner.ast

import scala.collection.mutable.Stack

	object RefProducer {

	var ref = 0;
  
	var empties = Stack[Int]();
  
	def free_ref(ref: Int) {
      empties.push(ref);
	}
  
	def get_ref(): Int = {
	  if (!empties.isEmpty) {
	    return empties.pop();
	  }
	  ref = ref + 1;
	  return ref;
	}
}