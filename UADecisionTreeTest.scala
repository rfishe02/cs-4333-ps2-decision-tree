
import scala.io.Source
import java.io.PrintWriter
import java.io.File

object UADecisionTreeTest {
  
  def main(args : Array[String]) {
    
    getRandomAccuracy("./weather.csv")
    
    var t = new UADecisionTree()
    t.setTreeMaxDepth(10)
    val data = t.getTrainingMatrix("./train.csv", "GOLF")
    
    var a = t.getSubSet(data.iterator, 0)
    
    for(item <- a) {
      println(item)
    }

    // Find first attribute to split on,
    // then use recursion to split on data.
    
    //t.train(data,0)
    
    
  }
  
  //--------------------------------------------------
  // Split the data into training data & test data.
  //
  // This approach is similiar to cluster sampling.
  // Every five lines is a cluster. We take one
  // line at random from that cluster. This produces
  // a rough 80/20 split.
  //--------------------------------------------------
  
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
  
}