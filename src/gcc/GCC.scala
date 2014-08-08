package gcc

object GCC extends App {

  import Expressions._
  import Translator.compile
  import Machine.execute
  
  def print(cs: Seq[Command]) {
    var i = 0
    cs foreach { x => println(f"$i%02d: $x%s"); i += 1 } 
  }
  
  def test1 {
	  // x2 x = x x +
	  // main = 21 x2 @
	  val example = List(
	      LDC(21),
	      LDF(4),
	      AP(1),
	      RTN,
	      LD(0,0),
	      LD(0,0),
	      ADD,
	      RTN)
	  print(example)
	  println()
	  println(execute(example))
	  println()
  }
  
  def test2 {
    val double = DEF { x => x * 2 }
    val main = DEF { x => 
      IF (x > 0) { 
        x + 1 
      } ELSE { 
        double(x - 1) 
      } 
    } 
    
    Translator.debug = true
    val cmain = Translator.compile(main)
	println()
    println(execute(cmain, -21))
    println()
    println(execute(cmain, +21))
    println()
  }
  
  def test3 {
    val ns = 1 :: 2 :: 3 :: 4 :: NIL
    val map = DEF { (f, xs) =>
      IF (atom(xs)) {
        xs
      } ELSE {
        f(xs.head) :: (recursive(f, xs.tail))
      }
    }
    val square = DEF { x => x * x }
    val main = DEF {
      map(square, ns)
    }
    
    Translator.debug = true
    val cmain = Translator.compile(main)
	println()
    println(execute(cmain))
  }

  test1
  test2
  test3
}





