package gcc

/*
 * Possible values for GCC processor:
 *  - Number
 *  - Pair
 *  - Closure
 */

trait Value {
  override def toString = this match {
    case Num(value)       => s"$value"
    case Pair(left,right) => s"($left,$right)"
    case Clos(addr,env)   => s"Closure@$addr"
  }
}

case class Num(value: Int) extends Value

case class Pair(left: Value, right: Value) extends Value

case class Clos(addr: Int, env: Env) extends Value

/*
 * Some implicit conversions
 */
object Value {
  
  implicit def value2int(v: Value) = v match {
    case Num(value) => value
    case _ => error(s"$v is expected to be a number")
  } 
  
  implicit def value2pair(v: Value) = v match {
    case p: Pair => p
    case _ => error(s"$v is expected to be a pair")
  } 
  
  implicit def int2value(v: Int) = Num(v)
  
  implicit def bool2value(v: Boolean) = Num(if (v) 1 else 0)

  implicit def value2bool(v: Value) = (value2int(v) != 0)

  type Values = Seq[Value]
}