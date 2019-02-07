
import scala.io.Source
import scala.collection.mutable.HashMap

import scala.collection.mutable.HashSet

class UADecisionTree {
  
  var prob : HashMap[String,HashMap[String,Int]] = _;
  var dist : HashMap[String,HashSet[String]] = _;
  var tot : Int = _;
  
  var depth : Int = _;
  var impur : Float = _;
  
  //----------------------------
  // Required methods
  //----------------------------
  
  def getTrainingMatrix(filename : String, target: String) = {
    
    // Get statistics about the file & store them.
    
    getStats(filename)
    
    // Use the statistics to perform the following steps.
    
    // Rearrange the data such that the target variable is placed at the
    // end of each record.
    
    // Test for attributes with more than 20 distinct values.
    
    val src = Source.fromFile(filename)
    val itr = src.getLines()
    val titles = itr.next().split(",") 
    
    
    
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
  
  //----------------------------
  // Custom methods
  //----------------------------
  
  def getStats(filename : String) = {
      
    // Open the file.
    
    val src = Source.fromFile(filename)
    val itr = src.getLines()
    val titles = itr.next().split(",") 
    
    // Create variables to store the data.
    
    var p = HashMap.empty[String,HashMap[String,Int]];
    var d = HashMap.empty[String,HashSet[String]]
    var t = 0
    
    for(attr <- titles) {
      
      p += (attr -> new HashMap[String,Int])
      
    }
    
    // Iterate through the contents of the file & calc frequencies.
    
    for(line <- itr) {
      
      var spl = line.split(",")
      
      for (ind <- 0 to spl.length-1) {
        
        if(p(titles(ind)).contains(spl(ind))) {
          
          
          
        } else {
          
          p(titles(ind)) += (spl(ind) -> 1)
          
        }
        
      }
      
    }
    
    // Save the statistics to the class.
    
    prob = p;
    dist = d;
    tot = t;

    src.close()
      
  }
  
  /*
    val src = Source.fromFile(filename).getLines.toList
    val test : Map[Char, List[String]] = src.groupBy(_.toString().charAt(0))
    
    println(test('0').length)
    println(test('1').length)
    
    for(line <- test){
      
      println(line)
      
    }
   */
  
}