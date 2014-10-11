object Decorators {
    
  object profile { 
    
    private var cm: Map[String, Int] =  Map().withDefault(_ => 0)

    def count(name: String) = 
      cm(name)
   
    def reset(name: String) = 
      cm += name -> 0

    def apply[A, B](name: String)(f: A => B) = new Function1[A, B] {
      def apply(x: A) = {
        val n = cm(name)
        cm += name -> (1 + n)
        f(x)
      }
    }
  }
 
  object trace {
    
      var pPipe = 0
    def apply[A, B](name: String)(f: A => B) : Function1[A, B] = new Function1[A, B] {
      // You may add more fields here

      def apply (x: A): B = {
       try {
	 for(i<-0 until pPipe) 
	    print("| ")
	 println(",- "+name+"("+x+")")
	 pPipe+=1
	 val re = f(x)
	 for(i <- 0 until (pPipe-1))
	    print("| ")
	 println("`- "+re)
	 pPipe = pPipe-1
	 re
         }
	catch {
	 case e: Exception => 
		pPipe = pPipe-1
		throw e
	}
     }
    }
  } 
  
  
  object memo {
      // You may add more fields here
    def apply[A, B](f: A => B) : Function1[A, B] = new Function1[A, B] {
      // You may add more fields here
	var map1: Map[A, Either[Throwable, B]] = Map() 
      def apply (x: A): B = {
	try {
	if(map1.contains(x)) {
		map1(x) match{
		case Left(e) => throw e
		case Right(n) => n
	} }
	else {
		val re = f(x)
		map1 += (x -> Right(re))
		re
	}
	
	}
	catch {
	case e: Exception =>
		map1 += (x -> Left(e))
		throw e
	}
		
    }
  }
}

}


