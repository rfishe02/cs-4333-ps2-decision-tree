
/********************************
Name: Renae Fisher
Username: 
Problem Set: PS2
Due Date: 02, 21, 2019
********************************/

import scala.collection.mutable.ListBuffer

class Node(c : Int) {
  
  var parent: Node = _;
  var children : ListBuffer[Node] = _;
  
  var attr : String = _;
  var subAttr : String =_;
  var res : String = _;

  var ent : Float = _;
  var col : Int = c;
  
  def setAttr(str : String) {
    
    val spl = str.split(",")
    attr = spl(0)
    
    if(spl.length > 1) {
      subAttr = spl(1)
    }
    
  }
  
  def getAttr() : String = {
    
    if(subAttr != null) {
      return attr+","+subAttr
    } else {
      return attr
    }
    
  }
  
  def setValues(str : String, e: Float) {
    
    val spl = str.split(",")
    attr = spl(0)
    
    if(spl.length > 1) {
      subAttr = spl(1)
    }
    
    ent = e
  }
  
  def addLink(c : Node) {
    
    if(children == null) {
      children = new ListBuffer[Node]()
    }
    
    children += c
    c.parent = this
    
  }
  
}