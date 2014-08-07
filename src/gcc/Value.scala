package gcc

trait Value

object Value {
  
  case class Num(value: Int) extends Value {
    override def toString = value.toString
  }

  case class Pair(left: Value, right: Value) extends Value {
    override def toString = s"($left,$right)"
  }
  
  case class Clos(addr: Int, env: Env) extends Value
  
  implicit def fromNum(v: Value) = v match {
    case Num(value) => value
    case _ => error(s"$v is expected to be a number")
  } 
  
  implicit def asPair(v: Value) = v match {
    case p: Pair => p
    case _ => error(s"$v is expected to be a pair")
  } 
  
  implicit def toNum(v: Int) = Num(v)
  
  implicit def toNum(v: Boolean) = Num(if (v) 1 else 0)

  type Values = Seq[Value]
}