
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

class UADecisionTree {

  var depth : Integer = _;
  var impur : Float = _;
  
  //------------------------------------------------
  // Necessary methods
  //------------------------------------------------
  
  def getTrainingMatrix(filename : String, target: String) : ArrayBuffer[ArrayBuffer[String]] = {
    
    // Count the number of sub-attributes.
    
    val stats = getStats(filename) // ( count, rows, cols )
    
    // Rearrange the data such that the target variable is at the end.

    val src = Source.fromFile(filename)
    val itr = src.getLines()
    
    val labels = itr.next().split(",")
    
    val matrix = ArrayBuffer.fill[ArrayBuffer[String]](stats._2)(null)
    
    var col : Integer = 0
    var row : Integer = 0
    var spl = Array[String]()
    
    for(line <- itr) {
      spl = line.split(",")
      col = 0
      
      matrix(row) = ArrayBuffer.fill[String](stats._3)(null)
      
      for(ind <- 0 to spl.length -1) {
        
        if(stats._1(ind) <= 20) {
          
          if(labels(ind).equalsIgnoreCase(target)){
            matrix(row)(matrix(row).length-1) = spl(ind)
          
          } else {
            matrix(row)(col) = spl(ind)
          
            col += 1
          }
        }
      }
      row += 1
    }
    
    printMatrix(matrix)
    
    return matrix
    
  }
  
  def setTreeMaxDepth(max : Integer) {
    depth = max
    
  }
  
  def setMinimumImpurity(min : Float) {
    impur = min
    
  }
  
  def train(data : Iterator[ArrayBuffer[String]], d : Integer) {
    
    // Must take into account max tree limit &
    // min impurity value.
    
    // Take training data as a parameter.
    
    if(d < depth) {

      // 1. Find the attribute with the largest IG. 
      //    this becomes node current.
      
      var maxCol : Integer = 0
      
      for(item <- data) {
        
        
        
      }
      
      // 2. Node current becomes an inner node.
      //    Split the results on this value & create child nodes.
      
      // 3. Remove current from the set of attributes for training.
      
      // 4. If the subset is pure, do not split. Otherwise,
      //    continue to recursively split the results on the child node.
      
      var subSet = getSubSet(data,maxCol)
      
      for( item <- subSet) {
        train(item.iterator,d+1);
        
      }
      
    }
    
  }
  
  def classifyValue(record : String) {
    
    
    
  }
  
  //------------------------------------------------
  // My methods
  //------------------------------------------------
  
  def getStats(filename : String) = {
    
    val s = HashMap.empty[String,HashSet[String]]
    val src = Source.fromFile(filename)
    val itr = src.getLines()
   
    // Create a new HashSet for each label.
    
    val attr = itr.next().split(",")
    
    for(item <- attr) {
      s+= (item -> new HashSet[String])
    }

    // Count the sub-attributes using the HashSet.
    
    val counts = new Array[Integer](attr.length)
    var r : Integer = 0
    
    var spl = Array[String]()
    
    for(line <- itr) {
      
      spl = line.split(",")
      
      for( ind <- 0 to spl.length - 1) {
        
        if(!s(attr(ind)).contains(spl(ind))) {
          s(attr(ind)) += (spl(ind))
          counts(ind) += 1
        }
        
      }
      
      r = r + 1
    }
    
    // Determine how many of the sub-attributes are valid.
    
    var c : Integer = 0
    
    for( ind <- 0 to counts.length-1) {
      if(counts(ind) <= 20) {
        c += 1
      }
    }

    (counts,r,c)
    
  }
  
  def getSubSet(m : Iterator[ArrayBuffer[String]], col : Integer) = {
    
    // Build subsets of data within a column.
    // This set doesn't include the column.
    
    val subSet = HashMap.empty[String,ArrayBuffer[ArrayBuffer[String]]]
    var tmp = ArrayBuffer[String]()
    
    for(item <- m) {

      if(!subSet.contains(item(col))) {
        subSet += (item(col) -> new ArrayBuffer[ArrayBuffer[String]]())
      }
      
      tmp = new ArrayBuffer[String]()
      
      for(ind <- 0 to item.length-1) {
        if(ind != col) {
          tmp += item(ind)
        }
      }
      
      subSet(item(col)) += tmp
    }

    (subSet.valuesIterator)
    
  }
  
  def printMatrix(matrix : ArrayBuffer[ArrayBuffer[String]]) {
    
    for( row <- 0 to matrix.length-1) {
      
      for( col <- 0 to matrix(row).length-1) {
        
        print(matrix(row)(col)+" ")
        
      }
      println()
      
    }
    
  }
  
}