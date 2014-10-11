import scala.collection.immutable.HashMap
import scala.util.matching.Regex
import Crypt._
import java.io.PrintWriter

case class Entry ( account   : String
                 , password  : String
                 , uid       : Int
                 , gid       : Int
                 , gecos     : String
                 , directory : String
                 , shell     : String
                 )

object Entry {
  
  def apply(line: String) : Entry = {
    val e = line.split(":")
    Entry(e(0),e(1),e(2).toInt, e(3).toInt, e(4),e(5),e(6)) 
  }

}

object Crack {

  def transformReverse(w: String) : Iterator[String] = {
    val lIt= List(w, w.reverse)
    Iterator(w, w.reverse)
  }
  
  def transformCapitalize(w: String) : Iterator[String] = {
    if(w.length == 0 )
      Iterator(w)
    else
      for(ch <-Iterator(w.substring(0,1).toUpperCase, 
                          w.substring(0,1).toLowerCase);
                        x <- transformCapitalize(w.substring(1)))
          yield( ch + x)
  }

  def helper(s: String) : Iterator[String] = {
    s match {
      case "o" => Iterator("o", "0")
      case "O" => Iterator("O", "0")
      case "z" => Iterator("z", "2")
      case "Z" => Iterator("Z", "2")
      case "a" => Iterator("a", "4")
      case "A" => Iterator("A", "4")
      case "b" => Iterator("b", "6", "8")
      case "B" => Iterator("B", "6", "8")
      case "g" => Iterator("g", "9")
      case "G" => Iterator("G", "9")
      case "q" => Iterator("q", "9") 
      case "Q" => Iterator("Q", "9")
      case "i" => Iterator("i", "1") 
      case "I" => Iterator("I", "1")
      case "l" => Iterator("l", "1") 
      case "L" => Iterator("L", "1")
      case "e" => Iterator("e", "3") 
      case "E" => Iterator("E", "3") 
      case "s" => Iterator("s", "5")
      case "S" => Iterator("S", "5") 
      case "t" => Iterator("t", "7")
      case "T" => Iterator("T", "7")
      case _   => Iterator(s)
  } 
}      
  
  def transformDigits(w:String) : Iterator[String] = {
       if(w.length == 0)
          Iterator(w)
       else
          for(ch<-helper(w.substring(0,1));
                x <- transformDigits(w.substring(1)))
          yield(ch+x)
  }

  def checkPassword(plain: String, enc: String) : Boolean = 
    Crypt.crypt(enc, plain) == enc
 
  def candidateWords(file: String) =
    Words.wordsMatchingRegexp(file, new Regex("""^.{6,8}$"""))

  // scala> Crack("passwd", "words", "soln.1")
  // goto> scala Crack passwd words soln.1
  def apply(pwdFile: String, wordsFile: String, outFile: String) : Unit = {
    var w = candidateWords(wordsFile)
    val pwd = Lines.iterator(pwdFile)
    val wf = Lines.iterator(wordsFile)
    val of = new PrintWriter(outFile)

    val x = for(a <- pwd) yield Entry.apply(a)
    var checker = false
    
    for(ps <- x) {
    checker = false 
    w = candidateWords(wordsFile)
    for(wps <- w) { 

      if(checkPassword(wps,ps.password)) {
      of.write(ps.account + "=" + wps + "\n")
        of.flush()
        checker =true
      }

  
    if(checker == false) { for(s <- transformReverse(wps)) {
      
  if(checkPassword(wps,ps.password)) {
        of.write(ps.account + "=" + wps + "\n")
        of.flush()
        checker = true
      } 

}
 }
    
  }
}
of.close()
}

  
  def main(args: Array[String]) = { 
    println("Begin: Cracking Passwords")
    apply(args(0), args(1), args(2))
    println("Done: Cracking Passwords")
  }
}

// vim: set ts=2 sw=2 et:

