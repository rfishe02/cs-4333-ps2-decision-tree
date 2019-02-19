
import scala.collection.mutable.ListBuffer

class Node(a : String, c : Int) {
  
  var parent: Node = _;
  var children : ListBuffer[Node] = _;

  var attr : String = a;
  var res : String = _;
  var col : Int = c;
  var ent : Float = c;
  
  def addChild(c : Node) {
    
    if(children == null) {
      children = new ListBuffer[Node]()
    }
    
    children += c
    
  }
  
}