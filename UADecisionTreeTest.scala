
import scala.io.Source
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.HashSet
import scala.collection.mutable.ArrayBuffer

object UADecisionTreeTest {
   
  def main(args : Array[String]) {
    
    val target = "PLAY GOLF"
    
    getRandomAccuracy("./titanicdata.csv")
    
    val t = new UADecisionTree()
    t.setTreeMaxDepth(10)
    t.setMinimumImpurity(0.05.toFloat)
    
    val data = t.getTrainingMatrix("./weathertest.csv", target)
   
    val s = new Node(data._1(0).length-1)
    s.setValues(target, data._2.toFloat)

    t.train(s,data._1,0)

    // Attempt to classify value.
    
    val map = HashSet.empty[String]
    val itr = data._1.iterator
    val tmp = itr.next()
    var spl = Array[String]()
    
    for(ind <- 0 to tmp.length-1) {
      spl = tmp(ind).split(",")
      map += tmp(ind)
      map += spl(0)
      print(tmp(ind)+" ")
    }
    println()

    t.traverse(s,map,0)
    
    
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
    
    // Accuracy = # of correct positions / total positions
    
    // Determine the best depth for the data set using the test
    // & training data.
    
    // Start with 0, & increase by 1 until you reach ten,
    // or the max depth.
    
    // Perform this test 100 times & print the average test results.
    
  }

}