
import scala.io.Source
import scala.collection.mutable.HashMap

import scala.collection.mutable.HashSet

class UADecisionTree {
  
  var prob : HashMap[String,HashMap[String,Int]] = _;
  var tot : Int = _;
  
  var attr : Array[String] = _;
  
  var depth : Int = _;
  var impur : Float = _;
  
  //----------------------------
  // Required methods
  //----------------------------
  
  def getTrainingMatrix(filename : String, target: String) : Array[Array[Int]] = {

    val data = getStats(filename) // Get data about the file.

    val src = Source.fromFile(filename)
    val itr = src.getLines()

    val titles = itr.next().split(",") 
    var at = orderAttr(data._2,data._4,titles,target) // Rearrange the titles for class storage.
    
    var tMatrix = buildTMatrix(itr,data._2,titles,target,data._3,data._4)// Iterate over the lines of the file & build the training matrix.

    // Assign values.
    
    prob = data._1
    tot = data._3
    attr = at
    
    return tMatrix
 
  }
  
  def setTreeMaxDepth(depth: Int) = {
    
    // Accepts an integer which is the maximum number of splits that can
    // take place.
    
    
    
  }
  
  def setMinimumImpurity(depth: Float) = {
    
    // Accepts a float which is the min impurity that must be 
    // satisfied before a node can be split.
    
    
    
  }
  
  def train() = {
    
    // Given a set of attributes, find the attribute with the largest IG.
    // This becomes Node current.
    
    // Node current becomes the current inner node.
    // Split the results on this value and create new child nodes.
    
    // Remove Node current from the set of available attributes for training.
    
    // If the subset is pure, do not continue to split. Otherwise, split
    // recursively the results on the child node(s).
    
    
    
  }
  
  def classifyValue(rec: String) = {
    
    // Should accepts a string that contains a record which paralells an
    // entry from the file.
    
    
    
  }
  
  //--------------------------------------------------
  // Get statistics about the file.
  //--------------------------------------------------
  
  def getStats(filename : String) = {
      
    val src = Source.fromFile(filename)
    val itr = src.getLines()
    
    val titles = itr.next().split(",")
    
    var freq = calcFreq(itr,titles) // Calculate the freqency of attribute subtypes.

    var valid = markValid(freq._1,titles) // Mark the attributes that have more than 20 subtypes.

    src.close()
    
    (freq._1,valid._1,freq._2,valid._2)
    
  }
  
  def calcFreq(itr : Iterator[String], titles: Array[String]) = {
    
    var p = HashMap.empty[String,HashMap[String,Int]]
    var spl = Array[String]()
    var t = 0
    
    for(item <- titles) {
      p += (item -> new HashMap[String,Int])
    }
    
    for(line <- itr) {
      spl = line.split(",")
      
      for (ind <- 0 to spl.length-1) {
 
        if(p(titles(ind)).contains(spl(ind))) {    
           p(titles(ind))(spl(ind))+=1
          
        } else {    
          p(titles(ind)) += (spl(ind) -> 1)
          
        }
      }
      t+=1;  // Count the total number of records.
    }
    
    (p,t)
  }
  
  def markValid(p: HashMap[String,HashMap[String,Int]], titles: Array[String]) = {
    
    var d = HashMap.empty[String,Boolean]
    var a = 0
    
    for(item <- titles) {
      
      if(p(item).size <= 20) {
        d += (item -> true)
        a += 1 // Count the number of valid attributes.
      } else {
        d += (item -> false)
        p -= item // Remove the freqency from the HashMap.
      }
      
    }
    
    (d,a)
    
  }
  
  //--------------------------------------------------
  // Use the statistics to get the data.
  //--------------------------------------------------
  
  def orderAttr(d: HashMap[String,Boolean], a: Int, titles: Array[String], t: String) : Array[String] = {
    
    var at = new Array[String](a)
    var ind = 0
    
    for(item <- titles) {
      
      try { 
        
        if(item.equalsIgnoreCase(t)) {
          at(at.length-1) = item
            
        } else {
            
          if(d(item)) {
            at(ind) = item  
            ind+=1
            
          }
            
        }
          
      } catch {
        case e: NoSuchElementException => e.printStackTrace()
        System.exit(1)
      }
         
    }
    
    return at

  }
  
  def buildTMatrix(itr: Iterator[String], d: HashMap[String,Boolean], titles: Array[String], target: String, t: Int, a: Int) : Array[Array[Int]] = {
    
    var tMatrix = Array.ofDim[Int](t,a)

    var col = 0
    var row = 0
    var spl = Array[String]()
    
    for(line <- itr) {
      
      spl = line.split(",")
      col = 0
      
      for (ind <- 0 to spl.length-1) {
 
        try {

          if(titles(ind).equalsIgnoreCase(target)) {
            tMatrix(row)(tMatrix(row).length-1) = spl(ind).toInt
            
          } else if(d(titles(ind))) {
            tMatrix(row)(col) = spl(ind).toInt
            
            col+=1;    
          }
          
         } catch {
           case e: NoSuchElementException => e.printStackTrace()
           System.exit(1)
         }
         
      }
      
      row+=1;
    }  
    
    return tMatrix;
  }
  
}