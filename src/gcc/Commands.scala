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

import Value._

// Load constant
case class LDC(const: Int) extends Command
// Load from environment
case class LD(frame: Int, ind: Int) extends Command

// Numeric operations
case object ADD  extends BinaryCommand(_ +  _)
case object SUB  extends BinaryCommand(_ -  _)
case object MUL  extends BinaryCommand(_ *  _)
case object DIV  extends BinaryCommand(_ /  _)
case object CEQ  extends BinaryCommand(_ == _)
case object CGT  extends BinaryCommand(_ >  _)
case object CGTE extends BinaryCommand(_ >= _)

// Check if value is number
case object ATOM extends UnaryCommand(_.isInstanceOf[Num])

// Create pair
case object CONS extends BinaryCommand(Pair)

// head and tail for pairs
case object CAR extends UnaryCommand(_.left)
case object CDR extends UnaryCommand(_.right)

// execute one of given branches
case class SEL(t: Int, f: Int) extends Command
// return back from branch
case object JOIN extends Command

// Create closure
case class LDF(addr: Int) extends Command
// Execute closure
case class AP(args: Int) extends Command
// Return from closure or program
case object RTN extends Command

// Create frame for tail-recursive call
case class DUM(args: Int) extends Command
// Call closure on DUM frame
case class RAP(args: Int) extends Command

