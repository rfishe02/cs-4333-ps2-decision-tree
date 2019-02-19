
import scala.collection.mutable.ListBuffer

class Node(a : String, c : Int) {
  
  var parent: Node = _;
  var children : ListBuffer[Node] = _;

  var attr : String = a;
  var col : Int = c;
  var res : String = _;
  var ent : Float = _;
  var ig : Float = _;
  
  def addChild(c : Node) {
    
    if(children == null) {
      children = new ListBuffer[Node]()
    }
    
    children += c
    
  }
  
}