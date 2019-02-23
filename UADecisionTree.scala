
/********************************
Name: Renae Fisher
Username: 
Problem Set: PS2
Due Date: 02, 21, 2019
********************************/

import scala.io.Source
import scala.collection.mutable.HashMap
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer

class UADecisionTree {

  var depth : Integer = _;
  var minImpur : Float = _;
  var start : Node = _; 
  var attr : Array[String] = _;

  //=============================================================
  // The methods used to set the depth & minimum impurity.
  //=============================================================
  
  def setTreeMaxDepth(max : Integer) {
    depth = max
    
  }
  
  def setMinimumImpurity(min : Float) {
    minImpur = min
    
  }
  
  //=============================================================
  // This method creates a training matrix from a file.
  //=============================================================
  
  def getTrainingMatrix(filename : String, target: String) = {
    
    val tgtFreq = HashMap.empty[String,Integer]
    var tmp = Array[String]()
    var e : Double = 0
    var i : Integer = 0

    // Determine the number of valid sub-attributes.
    
    val stats = getStats(filename, target) // ( data, s, c )
    val matrix = ListBuffer.empty[Array[String]]

    // Build the training matrix, and place the target variable at the end.
    
    val rawData = stats._1.iterator
    val attData = rawData.next()
    
    attr = attData // Store attribute information for the classifier.
    
    for( line <- rawData ) {
      i = 0
      tmp = new Array[String](stats._3)
      
      for(ind <- 0 until line.length) {
        
        if(stats._2(ind) <= 20) {
          
          if(attData(ind).equalsIgnoreCase(target)) { 
            tmp(tmp.length-1) = attData(ind)+","+line(ind) // Concat the attribute name to the sub-attribute.
            
            if(tgtFreq.contains(line(ind))) {
              tgtFreq(line(ind)) += 1 // Calculate the frequencies for the target variable.
            } else {
              tgtFreq += (line(ind) -> 1)
            }
            
          } else {
            tmp(i) = attData(ind)+","+line(ind)
            i += 1
          }
        }
      }
      matrix += tmp
    }

    // Calculate the base entropy for the target variable.
    
    for( subFreq <- tgtFreq ) {
      e += (subFreq._2.toDouble/matrix.size) * (scala.math.log10(subFreq._2.toDouble/matrix.size) / scala.math.log10(2))
    }
    
    e = e * -1
    
    (matrix,e)
  }
  
  //=============================================================
  // Get the statistics for the file.
  //=============================================================
  
  def getStats(filename : String, target : String) = {
    
    val rawData = ListBuffer.empty[Array[String]]
    val mapSet = HashMap.empty[String,HashSet[String]]

    val src = Source.fromFile(filename)
    val itr = src.getLines()
    
    val attrList = itr.next().split(",")
    var attrFreq = new Array[Integer](attrList.length)
    
    var spl = Array[String]()
    var c : Integer = 0

    // Create a new HashSet for each attribute, except for the target attribute.

    for(word <- attrList) {
      if(!word.equalsIgnoreCase(target)) {
        mapSet+= (word -> new HashSet[String])
      }
    }
    
    rawData += attrList // Store the attribute list for the getTrainingMatrix method.

    // Use the HashSet to get the frequency of each sub-attribute within an attribute.

    for(line <- itr) {
      spl = line.split(",")
      rawData += spl
      
      for( ind <- 0 until spl.length) {
        if(mapSet.contains(attrList(ind))) {
          
          if(!mapSet(attrList(ind)).contains(spl(ind))) {
            mapSet(attrList(ind)) += (spl(ind))
            attrFreq(ind) = attrFreq(ind) + 1
          }
          
        }
      }
    }
    
    // Count the number of valid subattributes.
    
    for(set <- mapSet.valuesIterator) {
      if(set.size <= 20) {
        c += 1
      }
    }
    
    c += 1 // Count the target variable, which wasn't counted.

    src.close()
    
    (rawData,attrFreq,c)
    
  }

  //=============================================================
  // This method recursively creates a decision tree from the training matrix.
  //=============================================================
  
  def train(parent : Node, data : ListBuffer[Array[String]], d : Integer) {

    if(d < depth) {
      
      // Find the attribute with the largest IG and return a node with children.
      val result = maxNode(parent,data)
      
      // Decide to recursively split, or stop.
      if(result != null) {
        
        parent.addLink(result)
        
        // Create a subset of data that doesn't include the max attribute.
        val set = getSubSet(data,result.col)
        
        for( n <- result.children) {
          
          if(n.ent > minImpur) {
            // Split using the subattribute's subset.
            
            train(n,set(n.getAttr()),d+1)
          } else {
            
            // The child becomes a leaf when the min impurity is sufficient.
            n.res = findOutcome(set(n.getAttr()))

          }
        }
      } else {
        
        // The parent becomes a leaf when the information gain isn't sufficient.
        parent.res = findOutcome(data)
        
      }
    } else {
      
      // Set the outcome for any nodes that reach the depth.
      parent.res = findOutcome(data)
      
    }
  }
  
  //=============================================================
  // Calculate the IG for the parent & generate child nodes.
  //=============================================================
  
  def maxNode(parent: Node, data : ListBuffer[Array[String]]) = {

    var maxSubAttr : Node = null;
    var parent : Node = null;
    var child : Node = null;
    
    var prob : Double = 0
    var condProb : Double = 0
    var ent : Double = 0
    var entSum : Double = 0
    var maxIG : Double = -1
    var currIG : Double = 0
 
    // Calculate the frequencies for the supplied data.
    val freq = calcFreq(data) // (attr,cond,sum)

    // Use the frequencies to find the information gain.
    for( key <- freq._1.keySet) {
      
      parent = new Node(key) // Create a new parent node.
      
      entSum = 0
      for( subAttrFreq <- freq._1(key)) {
        
        prob = subAttrFreq._2.toDouble / freq._3
        ent = 0

        for( condAttrFreq <- freq._2(subAttrFreq._1)) {
          condProb = condAttrFreq._2.toDouble / subAttrFreq._2
          ent += condProb * (scala.math.log10(condProb)/scala.math.log10(2))
        }

        ent = ent * -1
        
        child = new Node(key) // Create a new child & attach it to the parent.
        child.setValues(subAttrFreq._1, (ent).toFloat)
        parent.addLink(child)

        entSum += ent  * prob 
      }
      
      currIG = parent.ent-entSum
      if(currIG > maxIG) {
        maxIG = currIG
        maxSubAttr = parent
      }
      
      parent.attr = child.attr // Set the attribute of the parent.
    }
      
    (maxSubAttr)
    
  }
  
  //=============================================================
  // Calculate frequencies.
  //=============================================================
  
  def calcFreq(data : ListBuffer[Array[String]]) = {
    val attrFreq = HashMap.empty[Int,HashMap[String,Integer]] // Attribute
    val condFreq = HashMap.empty[String,HashMap[String,Integer]] // Conditional
    var tmp : String = ""
    var sum : Integer = 0 // Total lines
    
    for(line <- data) {
      for(ind <- 0 to line.size - 2) {
          
        // Add maps for attribute & conditional frequencies.
        
        if(!attrFreq.contains(ind)) {
          attrFreq += (ind -> HashMap.empty[String,Integer]) // Column --> ( Sub-Attribute --> Freq )
        }
        if(!condFreq.contains(line(ind))) {
          condFreq += (line(ind) -> HashMap.empty[String,Integer]) // Sub-Attribute --> ( Cond-Attribute --> Freq )
        }
        
        // Count attribute & conditional frequencies.

        if(attrFreq(ind).contains(line(ind))){
          attrFreq(ind)(line(ind)) += 1
        } else {
          attrFreq(ind) += (line(ind) -> 1)
        }
        
        tmp = line(ind)+""+line(line.size-1) // Concat the sub-attribute & the target sub-attribute.
        
        if(condFreq(line(ind)).contains( tmp ) ) {
          condFreq(line(ind))( tmp ) += 1
        } else {
          condFreq(line(ind)) += (tmp -> 1)
        } 

      }
      sum += 1
    }
    
    (attrFreq,condFreq,sum)
    
  }
  
  //=============================================================
  // Get the subsets of a column.
  //=============================================================
  
  def getSubSet(data : ListBuffer[Array[String]], col : Integer) = {
    
    val subAttrMap = HashMap.empty[String,ListBuffer[Array[String]]]
    var tmp = Array[String]()
    var i : Integer = 0
    
    // Build subsets of the data for each subattribute. 
    // This set won't include the column col.
    
    for(line <- data) {

      // Create a ListBuffer for the sub-attribute of column.
      
      if(!subAttrMap.contains(line(col))) {
        subAttrMap += (line(col) -> new ListBuffer[Array[String]]())
      }
      
      // Add items to an array, except the values within col.

      tmp = new Array[String](line.length-1)
      
      i = 0
      for(ind <- 0 until line.length) {
        if(ind != col) {
          tmp(i) = line(ind)
          i += 1
        }
      }
      
      // Add the array to the ListBuffer.
      subAttrMap(line(col)) += tmp
    }
    
    (subAttrMap)
    
  }
  
  //=============================================================
  // Find the outcome for a leaf node.
  //=============================================================

  def findOutcome(data : ListBuffer[Array[String]]) : String = {
    
    val freq = HashMap.empty[String,Integer]
    var label : String = null
    var max : Integer = 0
    
    // Get the frequencies of the subattributes in the set.
    
    for(line <- data) {
      
      if(freq.contains(line(line.length-1))) {
        freq(line(line.length-1)) += 1
      } else {
        freq += (line(line.length-1) -> 1)
      }
      
    }
    
    // Find the most frequent attribute & its associated outcome.
    
    for( values <- freq) {
      if(values._2 > max) {
        max = values._2
        label = values._1
      }
    }
    
    return label
    
  }
  
  //=============================================================
  // This method is used to test the application.
  //=============================================================
  
  def classifyValue(record : String) : Boolean = {
    
    val map = HashSet.empty[String]
    val tmp = record.split(",")
    var spl = Array[String]()
    
    // Format the record for classification & add its attributes to a HashSet.
    for(ind <- 0 until tmp.length) {
      map += attr(ind)
      map += attr(ind)+","+tmp(ind)
    }
    
    // Try to find the record in the decision tree.
    val res = recordOutcome(start,map)

    if(res == null) {
      return false
    } else if(tmp(tmp.length-1).equalsIgnoreCase(res.split(",")(1))) {
      return true
    } else {
      return false
    }
    
  }
  
  //=============================================================
  // Recursively travel the tree to find the outcome for a record.
  //=============================================================
  
  def recordOutcome(parent : Node, map : HashSet[String]) : String = {

    // Continue as long as the node doesn't have a survival condition.
    if(parent.res == null) {
      
      // Choose the child that has the same conditions as the record.
      var child : Node = null
      
      def findChild() {
        for(c <- parent.children) {
          if(map.contains(c.getAttr())) {
              child = c
              return
          }
        }
      }
      
      findChild()
      
      if(child != null) {
        recordOutcome(child,map)
      } else {
        // The outcome wasn't found. This would occur if the test set doesn't match the training set.
        return null 
      }
      
    } else {
      // The classification outcome was found, when the node has a non-null result.
      return parent.res
    }
    
  }
  
  //=============================================================
  // The following methods are used to debug the application.
  //=============================================================
  
  def printData(data : ListBuffer[Array[String]]) {

    for( item <- data) {
      for( col <- 0 until item.length) {
        print(item(col)+" ")
        
      }
      println()
      
    }
    
  }
  
  def printData(data : HashMap[String,ListBuffer[Array[String]]]) {

    for( item <- data) {
      println(item._1)
      for( array <- item._2) {
        for(col <- 0 until array.length) {
          print(array(col)+" ")
        }
        println()
      }
      println()
    }
    
  }
  
}