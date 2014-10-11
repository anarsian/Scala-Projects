import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import java.io.PrintWriter
import Lines._

object Words {

  def apply(file: String) : Iterator[String] =  {
      try {
        val i = iterator(file)
        for(w<-i) yield w.toLowerCase
      
       }
        catch {
          case e: java.io.FileNotFoundException => Iterator()
        }
  } 
  def groupFreq[A,B](xs: Iterator[A], f: A => B): HashMap[B, Int] = {
    var ta: HashMap[B, Int] = HashMap()
    for(x<-xs) {
      if(ta.contains(f(x)))
        ta+=(f(x) -> (ta(f(x)) + 1))
      else
        ta+= (f(x) -> 1)
    }
    ta
  }
 
  val inti = List(1,21,5,39,12,7,92)

  def isEven(x: Int): String =
    if ((x % 2) == 0) { "Even" } else { "Odd" } 

  def sizeFreq(file: String): HashMap[Int, Int] = {
    val i = iterator(file)
    groupFreq(i, (x: String) => x.size)

  }

  def charFreq(file: String): HashMap[Char, Int] = 
  {
    val chars   = for(it <- apply(file) ; ch <- it) yield ch
    val grouper = (x:Char) => x
    groupFreq(chars, grouper) 
  }

  def wordsOfSize(file: String, size: Int) : Iterator[String] = {
   val it = iterator(file)
   val ch = for(
              i <- it 
              if(i.length == size))
                yield i 
      
   ch

  }

  def wordsWithAllVowels(file: String): Iterator[String] =
  { 
    Words(file).filter((s:String) =>(s.contains("a") && 
      s.contains("e") && s.contains("i") && s.contains("o") && s.contains("u")))
  }
  def wordsWithNoVowels(file: String): Iterator[String] = 
  {
    Words(file).filter((s:String) =>(!s.contains("a") && 
      !s.contains("e") && !s.contains("i") && !s.contains("o") && 
                    !s.contains("u")))
  }
  def wordsMatchingRegexp(file: String, re: Regex): Iterator[String] =
  { 
       
    Words(file).filter((s:String) => !((re findAllIn s).isEmpty)) 
  }
}

// vim: set ts=2 sw=2 et:

