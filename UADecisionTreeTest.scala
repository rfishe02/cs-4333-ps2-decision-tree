
import scala.io.Source
import java.io.PrintWriter
import java.io.File
import scala.collection.mutable.HashSet
import scala.collection.mutable.ListBuffer
import java.io.FileNotFoundException
import java.io.IOException

object UADecisionTreeTest {
   
  def main(args : Array[String]) {

    val target = "SURVIVED"
    val filename = "./titanicdata.csv"
    val depth = 10
    val impur = 0.05F
    val trials = 100
    
    getBestDepth(filename,target, depth, impur, trials)
    
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
  
  def getRandomAccuracy(filename : String, target:String) {
    
    val src = Source.fromFile(filename)
    val itr = src.getLines()
    var spl = Array[String]()
    var tmp : String = null
    
    val testWrite = new PrintWriter(new File("./test.csv"))
    val trainWrite = new PrintWriter(new File("./train.csv"))
    
    val labels = itr.next()
    var attr = labels.split(",")
    
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
        
        // Move the target variable to the end of the test data.
        spl = line.split(",")
        
        for(i <- 0 to spl.length-1) {
          if(attr(i).equalsIgnoreCase(target)) {
            tmp = spl(i)
          } else {
            testWrite.print(spl(i)+",")
          }
        }

        testWrite.print(tmp)
        testWrite.println()
        
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
  
  def getBestDepth(filename : String, target: String, d : Integer, impurity : Float, trials : Integer) {
    
    // Accuracy = # of correct positions / total positions
    
    // Determine the best depth for the data set using the test
    // & training data.
    
    // Start with 0, & increase by 1 until you reach ten,
    // or the max depth.
    
    // Perform this test 100 times & print the average test results.

    var tree : UADecisionTree = null
    var testData = (ListBuffer[Array[String]](),0.0)
    var start : Node = null
    val correct = new Array[Integer](d+1)
    val total = new Array[Integer](d+1)
    
    try {
      val verified = verifyTarget(filename, target)
      
      if(verified) {
        // Conduct tests.
    
        for(depth <- 0 to d) {
          correct(depth) = 0
          
          for(a <- 1 to trials) {
        
            getRandomAccuracy(filename,target)
        
            tree = new UADecisionTree()
            tree.setTreeMaxDepth(depth)
            tree.setMinimumImpurity(impurity)
    
            testData = tree.getTrainingMatrix("./train.csv", target)

            tree.start = new Node(testData._1(0).length-1)
            tree.start.setValues(target,testData._2.toFloat)
          
            tree.train(tree.start,testData._1,0)
          
            val src = Source.fromFile("./test.csv")
            val itr = src.getLines()
    
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
        for(ind <- 0 until correct.length) {
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