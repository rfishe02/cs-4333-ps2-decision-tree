
import scala.io.Source
import java.io.PrintWriter
import java.io.File

object test {
  
  def main(args:Array[String]) { 
    
    splitData("./titanicdata.csv");
    
    val t = new UADecisionTree();
    var tMatrix = t.getTrainingMatrix("./train.csv", "SURVIVAL")
    
    for(row <- tMatrix) {
      
      for(col <- row) {
        
        print(col+" ")
        
      }
      
      println()
    }
    
  }
  
  def getRandomAccuracy() = {
    
  }
  
  def getBestDepth() = {
    
  }
  
  //--------------------------------------------------
  // Split the data into training data & test data.
  //
  // This approach is similiar to cluster sampling.
  // Every five lines is a cluster. We take one
  // line at random from that cluster. This produces
  // a rough 80/20 split.
  //--------------------------------------------------
  
  def splitData(filename : String) = {
    
    val src = Source.fromFile(filename)
    val itr = src.getLines()
    val titles = itr.next()
    
    val testWrite = new PrintWriter(new File("./test.csv" ))
    val trainWrite = new PrintWriter(new File("./train.csv" ))
    
    testWrite.println(titles)
    trainWrite.println(titles)
    
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
        step = 1;
        gate = true;
      }
      
      step+=1;
    }
    
    src.close();
    testWrite.close();
    trainWrite.close();
    
  }
  
}