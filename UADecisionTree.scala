
import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

class UADecisionTree {

  var depth : Integer = _;
  var impur : Float = _;
  
  //=============================================================
  // The necessary methods.
  //=============================================================
  
  //=============================================================
  // This method builds a training matrix from the training data.
  //=============================================================
  
  def getTrainingMatrix(filename : String, target: String) = {
    
    val tgt = HashMap.empty[String,Integer]
    var e : Double = 0
    var c : Integer = 0
    var spl = Array[String]()
    var tmp = Array[String]()

    // Determine which subattributes are valid.
    
    val stats = getStats(filename, target) // ( data, s, c )
    val matrix = ListBuffer.empty[Array[String]]

    // Rearrange the data such that the target variable is at the end.
    
    val data = stats._1.iterator
    val attData = data.next()
    
    for( item <- data ) {
      c = 0
      
      tmp = new Array[String](stats._3)
      
      for(col <- 0 to item.length - 1) {
        
        if(stats._2(col) <= 20) {
          
          if(attData(col).equalsIgnoreCase(target)) { 
            tmp(tmp.length-1) = item(col)
            
            // Calculate the frequencies for the target variable.
            if(tgt.contains(item(col))) {
              tgt(item(col)) += 1
            } else {
              tgt += (item(col) -> 1)
            }
            
          } else {
            tmp(c) = item(col)
            c += 1
          }
          
        }
      }
      matrix += tmp
      
    }

    //printData(matrix)
    // Calculate base entropy for the target variable.
    
    for( item <- tgt) {
      e += (item._2.toDouble/matrix.size) * (scala.math.log10(item._2.toDouble/matrix.size) / scala.math.log10(2))
    }
    
    e = e * -1
    
    (matrix,e)
  }
  
  //=============================================================
  // These are the setters for detph & impurity.
  //=============================================================
  
  def setTreeMaxDepth(max : Integer) {
    depth = max
    
  }
  
  def setMinimumImpurity(min : Float) {
    impur = min
    
  }
  
  //=============================================================
  // This is the train method.
  //=============================================================
  
  def train(data : Iterator[Array[String]], d : Integer) {
    
    // Must take into account max tree limit &
    // min impurity value.
    
    // Take training data as a parameter.
    
    if(d < depth) {

      // 1. Find the attribute with the largest IG. 
      //    this becomes node current.
      
      // 2. Node current becomes an inner node.
      //    Split the results on this value & create child nodes.
      
      // 3. Remove current from the set of attributes for training.
      
      // 4. If the subset is pure, do not split. Otherwise,
      //    continue to recursively split the results on the child node.

      
    }
    
  }
  
  //=============================================================
  // This method is used to test the application.
  //=============================================================
  
  def classifyValue(record : String) {
    
    // This method is used during the test phase.
    
  }
  
  //=============================================================
  // Methods I've developed.
  //=============================================================
  
  //=============================================================
  // Get the statistics for the file.
  //=============================================================
  
  def getStats(filename : String, target : String) = {
    
    val data = ListBuffer.empty[Array[String]]
    val set = HashMap.empty[String,HashSet[String]]

    val src = Source.fromFile(filename)
    val itr = src.getLines()
    val attr = itr.next().split(",")
    
    var spl = Array[String]()
    var s = new Array[Integer](attr.length)
    var c : Integer = 0

    // Create a new HashSet for each attribute, except the target.

    for(word <- attr) {
      if(!word.equalsIgnoreCase(target)) {
        set+= (word -> new HashSet[String])
      }
    }
    
    data += attr // This is used as a reference when building the training matrix.

    // Get the count of sub-attributes using the HashSet.

    for(line <- itr) {
      spl = line.split(",")
      data += spl
      
      for( col <- 0 to spl.length - 1) {
        if(set.contains(attr(col))) {
          if(!set(attr(col)).contains(spl(col))) {
            set(attr(col)) += (spl(col))
            s(col) = s(col) + 1
          }
        }
      }
    }
    
    // Count the number of valid subattributes.
    
    for(item <- set.valuesIterator) {
      if(item.size <= 20) {
        c += 1
      }
    }
    
    c += 1 // This is the target variable.

    src.close()
    
    (data,s,c)
    
  }
  
  //=============================================================
  // Get the subsets of a column.
  //=============================================================
  
  def getSubSet(data : ListBuffer[Array[String]], col : Integer) = {
    
    val subSet = HashMap.empty[String,ListBuffer[Array[String]]]
    var tmp = Array[String]()
    var i : Integer = 0
    
    // Build subsets of the data within a column. This set doesn't include the column.
    
    for(item <- data) {

      // Create a ListBuffer for each sub-attribute of a column.
      
      if(!subSet.contains(item(col))) {
        subSet += (item(col) -> new ListBuffer[Array[String]]())
      }
      
      // Add items to an array, except the values within col.

      tmp = new Array[String](item.length-1)
      
      i = 0
      for(c <- 0 to item.length-1) {
        if(c != col) {
          tmp(i) = item(c)
          i += 1
        }
      }
      
      // Add the array to the ListBuffer.
      subSet(item(col)) += tmp
    }

    printData(subSet)
    
    (subSet.valuesIterator)
    
  }
  
  //=============================================================
  // Calculate frequencies.
  //=============================================================
  
  def calcFreq(data : ListBuffer[Array[String]]) = {
    val freq = HashMap.empty[String,HashMap[String,Integer]] // Conditional
    val tot = HashMap.empty[Int,HashMap[String,Integer]] // Attribute
    var tmp : String = ""
    var sum : Integer = 0 // Total records
    
    for( item <- data) {
      for(col <- 0 to item.size - 2) {
          
        // Add maps for conditional & attribute frequencies.
        
        if(!freq.contains(item(col))) {
          freq += (item(col) -> HashMap.empty[String,Integer])
        }
        if(!tot.contains(col)) {
          tot += (col -> HashMap.empty[String,Integer])
        }
        
        // Calculate conditional & attribute frequencies.

        tmp = item(col)+""+item(item.size-1)
        
        if(freq(item(col)).contains( tmp ) ) {
          freq(item(col))( tmp ) += 1
        } else {
          freq(item(col)) += ( tmp -> 1)
        } 

        if(tot(col).contains(item(col))){
          tot(col)(item(col)) += 1
        } else {
          tot(col) += (item(col) -> 1)
        }
        
      }
      sum += 1
    }
    
    /*
    for( item <- freq) {
      println(item)
    }
    for( item <- tot) {
      println(item)
    }
    System.out.println(sum)*/
   
    (freq,tot,sum)
    
  }
  
  //=============================================================
  // Calculate information gain.
  //=============================================================
  
  def calcIG(parent: Double, attr: HashMap[Int,HashMap[String,Integer]], cond : HashMap[String,HashMap[String,Integer]], sum : Integer) = {

    var maxIG : Double = 0
    var cIG : Double = 0
    var maxCol : Int = 0
    var maxEnt : Double = 0
    
    var prob : Double = 0
    var cProb : Double = 0
    var ent : Double = 0
    var entSum : Double = 0
    
    for( key <- attr.keySet) {
      
      entSum = 0
      for( a <- attr(key)) {
        prob = a._2.toDouble / sum
        ent = 0
        
        for( b <- cond(a._1)) {
          cProb = b._2.toDouble / a._2
          
          if(cProb > 0) {
            ent += cProb * (scala.math.log10(cProb)/scala.math.log10(2))
          } else {
            ent += 0 
          }

        }
        
        ent = ent * -1 * prob 
        entSum += ent
      }
      cIG = parent-entSum
      
      if(cIG > maxIG) {
        maxIG = cIG
        maxCol = key
        maxEnt = 0
      }
    }
    
    //println(maxCol)
    
    (maxCol,maxEnt)
    
  }

  //=============================================================
  // The following methods are used to debug the application.
  //=============================================================
  
  def printData(data : ListBuffer[Array[String]]) {
    
    // Print the contents of the training matrix.
    
    for( item <- data) {
      
      for( col <- 0 to item.length-1) {
        
        print(item(col)+" ")
        
      }
      println()
      
    }
    
  }
  
  def printData(data : HashMap[String,ListBuffer[Array[String]]]) {
    
    // Print the contents of the subset.
    
    for( item <- data) {
      println(item._1)
      for( array <- item._2) {
        for(col <- 0 to array.length - 1) {
          print(array(col)+" ")
        }
        println()
      }
      println()
    }
    
  }
  
}