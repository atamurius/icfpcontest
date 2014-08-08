package gcc

object Expressions {

  /*
   * High level constructions on top of GCC commands
   */
  
  class Wrapper(val command: Command) extends Expr
  
  case class Number(n: Int) extends Wrapper(LDC(n))
  case class Local(n: Int)  extends Wrapper(LD(0,n))
  
  case class Unary(op: UnaryCommand, arg: Expr) extends Expr
  case class Binary(op: BinaryCommand, left: Expr, right: Expr) extends Expr

  case class If(cond: Expr, t: Expr, f: Expr) extends Expr
  
  case class Fun(argc: Int, body: Expr) extends Expr

  case class Call(f: Expr, args: Seq[Expr]) extends Expr
  
  case object Recursive extends Expr

  trait Expr {
    def +  (that: Expr) = Binary(ADD, this, that)
    def -  (that: Expr) = Binary(SUB, this, that)
    def *  (that: Expr) = Binary(MUL, this, that)
    def /  (that: Expr) = Binary(DIV, this, that)
    def == (that: Expr) = Binary(CEQ, this, that)
    def >  (that: Expr) = Binary(CGT, this, that)
    def >= (that: Expr) = Binary(CGTE, this, that)
    def <  (that: Expr) = !(this >= that)
    def <= (that: Expr) = !(this > that)
    def unary_! = (this == const(0))
    def head = Unary(CAR, this)
    def tail = Unary(CDR, this)
    def :: (that: Expr) = Binary(CONS, that, this)
    
    override def toString = this match {
      case Number(n) => s"$n"
      case Local(n) => s"$$$n"
      case Unary(op,arg) => s"$op($arg)"
      case Binary(op,a,b) => s"($a $op $b)"
      case If(c,t,f) => s"if ($c) $t else $f"
      case Call(f,args) => s"$f(${args mkString ","})"
      case Fun(a, body) => s"FUN$a[$body]"
      case Recursive => "RECURSIVE"
    }
    
    def apply(args: Expr*) = this match {
      case Fun(argc, _) if (argc != args.size) => 
        error(s"function $this requires $argc arguments")
      case _ => Call(this, args)
    }
  }

  implicit def const(value: Int)= Number(value)

  def atom(e: Expr) = Unary(ATOM, e)
  
  val NIL = Number(0)
  
  val recursive = Recursive  

  def IF(cond: Expr)(t: Expr) = new Object {
    def ELSE(f: Expr) = If(cond, t, f)
  }

  object DEF {
    def apply(f:                                   Expr) = Fun(0, f)
    def apply(f: (Expr)                         => Expr) = Fun(1, f(Local(0)))
    def apply(f: (Expr, Expr)                   => Expr) = Fun(2, f(Local(0), Local(1)))
    def apply(f: (Expr, Expr, Expr)             => Expr) = Fun(3, f(Local(0), Local(1), Local(2)))
    def apply(f: (Expr, Expr, Expr, Expr)       => Expr) = Fun(4, f(Local(0), Local(1), Local(2), Local(3)))
    def apply(f: (Expr, Expr, Expr, Expr, Expr) => Expr) = Fun(5, f(Local(0), Local(1), Local(2), Local(3), Local(4)))
  }
}










