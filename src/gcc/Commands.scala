package gcc

trait Command extends Product {
  override def toString = (productPrefix :: productIterator.toList) mkString " "
}

abstract class UnaryCommand(f: Value => Value) extends Command {
  def apply(a: Value) = f(a)
}

abstract class BinaryCommand(f: (Value, Value) => Value) extends Command {
  def apply(a: Value, b: Value) = f(a, b)
}

object Commands {

  case class LDC(const: Int) extends Command
  case class LD(frame: Int, ind: Int) extends Command

  import Value._
  
  case object ADD  extends BinaryCommand(_ +  _)
  case object SUB  extends BinaryCommand(_ -  _)
  case object MUL  extends BinaryCommand(_ *  _)
  case object DIV  extends BinaryCommand(_ /  _)
  case object CEQ  extends BinaryCommand(_ == _)
  case object CGT  extends BinaryCommand(_ >  _)
  case object CGTE extends BinaryCommand(_ >= _)

  case object ATOM extends UnaryCommand(_.isInstanceOf[Num])

  case object CONS extends BinaryCommand(Pair)

  case object CAR extends UnaryCommand(_.left)
  case object CDR extends UnaryCommand(_.right)

  case class  SEL(t: Int, f: Int) extends Command
  case object JOIN extends Command

  case class  LDF(addr: Int) extends Command
  case class  AP(args: Int) extends Command
  case object RTN extends Command

  case class DUM(frames: Int) extends Command
  case class RAP(args: Int) extends Command
}
