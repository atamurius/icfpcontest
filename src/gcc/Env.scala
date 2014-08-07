package gcc

import Value._

case class Env(parent: Env, data: Values) {

  def apply(fr: Int, ind: Int): Value =
    if (fr == 0) {
      if (data isDefinedAt ind) data(ind)
      else error(s"No env value $ind in $this")
    } 
    else if (parent == null)
      error(s"Invalid env index, overflow: $fr")
    else parent(fr - 1, ind)

  override def toString =
    data.mkString("[", ",", "]") + (
      if (parent == null) ""
      else "~" + parent)
}
