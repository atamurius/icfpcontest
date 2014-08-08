package gcc

import Value._

trait Env {
  def apply(fr: Int, ind: Int): Value
  
  def extend(data: Values) = SubEnv(this, data)
}

object EmptyEnv extends Env {
  def apply(fr: Int, ind: Int): Value = error(s"Invalid env, overflow by $fr") 
  
  override def toString = "EMPTY"
}

case class SubEnv(parent: Env, data: Values) extends Env {

  def apply(fr: Int, ind: Int): Value =
    if (fr == 0) {
      if (data isDefinedAt ind) data(ind)
      else error(s"No env value $ind in $this")
    } 
    else parent(fr - 1, ind)

  override def toString =
    data.mkString("[", ",", "]") + (
      if (parent == EmptyEnv) ""
      else "~" + parent)
}
