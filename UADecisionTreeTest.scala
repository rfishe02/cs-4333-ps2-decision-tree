
import scala.io.Source
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import java.io.FileNotFoundException
import java.io.IOException

object UADecisionTreeTest {
   
  def main(args : Array[String]) {

    val target = "PLAY GOLF"
    val filename = "./weather.csv"
    
    getBestDepth(filename,target)
    
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
  
  def getBestDepth(filename : String, target: String) {
    
    // Accuracy = # of correct positions / total positions
    
    // Determine the best depth for the data set using the test
    // & training data.
    
    // Start with 0, & increase by 1 until you reach ten,
    // or the max depth.
    
    // Perform this test 100 times & print the average test results.
    
    var impurity : Float = 0.05F
    var tree : UADecisionTree = null
    var testData = (ListBuffer[Array[String]](),0.0)
    var start : Node = null
    val correct = new Array[Integer](10)
    val total = new Array[Integer](10)
    
    try {

      val verified = verifyTarget(filename, target)
      
      if(verified) {
        
        // Conduct tests.
    
        for(depth <- 0 to 9) {
          correct(depth) = 0
          
          for(a <- 0 to 2) {
        
            getRandomAccuracy(filename)
        
            tree = new UADecisionTree()
            tree.setTreeMaxDepth(depth)
            tree.setMinimumImpurity(impurity)
    
            testData = tree.getTrainingMatrix("./train.csv", target)

            tree.start = new Node(testData._1(0).length-1)
            tree.start.setValues(target,testData._2.toFloat)
          
            tree.train(tree.start,testData._1,depth)
          
            val src = Source.fromFile("./test.csv")
            val itr = src.getLines()
            itr.next()
    
            for(data <- itr) {
              if(tree.classifyValue(data)){
                correct(depth)+=1
              }
              total(depth)+=1
            }
          }  
        }
      
        // Print results.
        var result : Double = 0
        for(ind <- 0 to correct.length-1) {
          result = correct(ind).toDouble / total(ind)
          System.out.println(ind+" "+result)
        }
        
      } else {
        System.out.println("Target attribute not found in data set.");
      }
      
    } catch {
     
      case ex : FileNotFoundException => {
        System.out.println("File not found.");
      }
      
      case ex: IOException => {
        ex.printStackTrace()
      }
      
    }
    
  }
  
  //=============================================================
  // Methods I've developed.
  //=============================================================
  
  def verifyTarget(filename: String, target : String) : Boolean = {
    
    var res = true
    
    val src = Source.fromFile(filename)
    val itr = src.getLines()
    val attr = itr.next().split(",")
    
    def findAttr() {
        
      for(word <- attr) {
        if(target.equalsIgnoreCase(word)) {
              return
        }
      }
      
      res = false

    }
      
    findAttr()

    return res
  }

}