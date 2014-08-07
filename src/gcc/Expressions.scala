package gcc

import Commands._

object Expressions {

  case class Cs(cs: Seq[Command]) extends Expr
  case class Un(op: UnaryCommand, arg: Expr) extends Expr
  case class Bi(op: BinaryCommand, left: Expr, right: Expr) extends Expr
  case class If(cond: Expr, t: Expr, f: Expr) extends Expr
  case class Ap(f: Expr, args: Seq[Expr]) extends Expr

  implicit def command(c: Command): Expr = Cs(Seq(c))
  implicit def const(value: Int): Expr = LDC(value)

  trait Expr {
    def +  (that: Expr) = Bi(ADD, this, that)
    def -  (that: Expr) = Bi(SUB, this, that)
    def *  (that: Expr) = Bi(MUL, this, that)
    def /  (that: Expr) = Bi(DIV, this, that)
    def == (that: Expr) = Bi(CEQ, this, that)
    def >  (that: Expr) = Bi(CGT, this, that)
    def >= (that: Expr) = Bi(CGTE, this, that)
    def <  (that: Expr) = !(this >= that)
    def <= (that: Expr) = !(this > that)
    def unary_! = (this == const(0))
    def head = Un(CAR, this)
    def tail = Un(CDR, this)
    def :: (that: Expr) = Bi(CONS, that, this)
  }

  def arg(n: Int): Expr = LD(0, n)
  val NIL: Expr = 0

  def IF(cond: Expr)(t: Expr) = new Object {
    def ELSE(f: Expr) = If(cond, t, f)
  }

  case class Fun(val arity: Int, val body: Expr) {
    def apply(args: Expr*) = Ap(body, args)
  }

  object DEF {
    def apply(f: Expr) = Fun(0, f)
    def apply(f: (Expr) => Expr) = Fun(1, f(arg(0)))
    def apply(f: (Expr, Expr) => Expr) = Fun(1, f(arg(0), arg(1)))
    def apply(f: (Expr, Expr, Expr) => Expr) = Fun(1, f(arg(0), arg(1), arg(2)))
    def apply(f: (Expr, Expr, Expr, Expr) => Expr) = Fun(1, f(arg(0), arg(1), arg(2), arg(3)))
    def apply(f: (Expr, Expr, Expr, Expr, Expr) => Expr) = Fun(1, f(arg(0), arg(1), arg(2), arg(3), arg(4)))
  }
}
