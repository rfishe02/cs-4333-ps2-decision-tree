
import scala.collection.mutable.ListBuffer

class Node(a : String, c : Int, e : Double) {
  
  var parent: Node = _;
  var children : ListBuffer[Node] = _;

  var attr : String = a;
  var col : Int = c;
  var ent : Double = e;
  var ig : Double = _;
  
  def addChild(c : Node) {
    
    if(children == null) {
      children = new ListBuffer[Node]()
    }
    
    children += c
    
  }
  
}