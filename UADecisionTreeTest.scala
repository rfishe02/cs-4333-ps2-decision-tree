
import scala.io.Source
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.HashMap
import scala.collection.mutable.ArrayBuffer

object UADecisionTreeTest {
  
  def main(args : Array[String]) {
    
    getRandomAccuracy("./weather.csv")
    
    val t = new UADecisionTree()
    t.setTreeMaxDepth(10)
    
    val data = t.getTrainingMatrix("./weather.csv", "PLAY GOLF")
    
    val sub = t.getSubSet(data, 0)
    
    val freq = t.calcFreq(sub.next())
    
    //val maxCol = getFirstSplit(parent,child)

    //t.train(data.iterator,0)
    
  }
  
  //=============================================================
  // The necessary methods.
  //=============================================================
  
  //=============================================================
  // Split the data into training data & test data.
  //
  // This approach is similiar to cluster sampling.
  // Every five lines is a cluster. We take one
  // line at random from that cluster. This produces
  // a rough 80/20 split.
  //=============================================================
  
  def getRandomAccuracy(filename : String) {
    
    val src = Source.fromFile(filename)
    val itr = src.getLines()
    val labels = itr.next()
    
    val testWrite = new PrintWriter(new File("./test.csv"))
    val trainWrite = new PrintWriter(new File("./train.csv"))
    
    testWrite.println(labels)
    trainWrite.println(labels)
    
    val rand = new scala.util.Random
    
    var roll : Integer = 0
    var step : Integer = 1
    var gate : Boolean = true
    
    for(line <- itr) {
      
      if(gate) {
        roll = rand.nextInt(5)+1
        gate = false
      }
      
      if(step == roll) {
        testWrite.println(line)
      } else {
        trainWrite.println(line)
      }
      
      if(step > 5) {
        step = 1
        gate = true
      }
      
      step += 1
      
    }
    
    src.close()
    testWrite.close()
    trainWrite.close()
    
  }
  
  def getBestDepth() {
    
    // Determine the best depth for the data set using the test
    // & training data.
    
    // Start with 0, & increase by 1 until you reach ten,
    // or the max depth.
    
    // Perform this test 100 times & print the average test results.
    
  }
  
  //=============================================================
  // Methods I've developed.
  //=============================================================
  
  def getFirstSplit(parent : (HashMap[Int,HashMap[String,Integer]],Integer), child : (HashMap[String,HashMap[String,Integer]],HashMap[String,Integer])) : Integer = {
    
    var cIG : Double = 0
    var maxIG : Double = 0
    var maxEnt : Double = 0
    var maxCol : Integer = 0
    
    var pProb : Double = 0
    var entParent : Double = 0
    
    var cProb : Double = 0
    var entChild : Double = 0
    var entChildSum : Double = 0

    for(key <- parent._1.keySet) {

      entParent = 0
      entChildSum = 0
      for(p <- parent._1(key)) {
        pProb = p._2.toDouble / parent._2
        entParent = entParent + pProb * (scala.math.log10(pProb)/scala.math.log10(2))
        
        entChild = 0
        for(c <- child._1(p._1)) {
          cProb = c._2.toDouble / p._2
          entChild = entChild + cProb * (scala.math.log10(cProb)/scala.math.log10(2))
          
        }
        entChild = pProb * entChild * -1
        entChildSum += entChild
        
      }
      
      entParent = entParent * -1
      cIG = entParent - entChildSum;

      if(cIG > maxIG) {
        maxIG = cIG
        maxCol = key
      }
      
    }

    return maxCol
    
  }
  
}