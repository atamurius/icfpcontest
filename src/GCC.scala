object GCC extends App {

  import collection.immutable._

  // --- values ---------------------------------------------------------------
  
  trait Value
  
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

  type Values = List[Value]

  // --- Commands -------------------------------------------------------------
  
  trait Command extends Product {
    override def toString = (productPrefix :: productIterator.toList) mkString " "
  }
  abstract class UnaryCommand(f: Value => Value) extends Command {
    def apply(a: Value) = f(a)
  }

  abstract class BinaryCommand(f: (Value,Value) => Value) extends Command {
    def apply(a: Value, b: Value) = f(a,b)
  }
  
  object Commands {
  
      case class LDC(const: Int) extends Command
      case class LD(frame: Int, ind: Int) extends Command
    
      case object ADD  extends BinaryCommand(_ +  _)
      case object SUB  extends BinaryCommand(_ -  _)
      case object MUL  extends BinaryCommand(_ *  _)
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
      
      case object STOP extends Command
  }
  import Commands._
 
  // --- environment ----------------------------------------------------------
  
  case class Env(parent: Env, data: Values) {
      def apply(fr: Int, ind: Int): Value =
        if (fr == 0) data(ind)
        else if (parent == null) error("Invalid environment index")
        else parent(fr - 1, ind)
      
      override def toString = "ENV"+ data.mkString("[",",","]")
  }
  
  // --- machine --------------------------------------------------------------
  
  class Machine(val code: List[Command], args: Values = Nil) {
    
      var data: Values = Nil
      
      var codePtr = 0
      
      var env = Env(null, args)
      
      var envs: List[Env] = List(null)
      
      var rets: List[Int] = List(-2)
      
      import Commands._
      
      def push(value: Value) = 
        data ::= value
      def pop = data match {
        case x :: rest => data = rest; x
        case Nil => error("No data")
      }
      
      def next(): Unit = {
        code(codePtr) match {
          case LDC(value) => 
            push(value)
          case LD(fr,ind) => 
            push(env(fr,ind))
          case command: UnaryCommand => 
            push(command(pop))
          case command: BinaryCommand => 
            push(command(pop,pop))
          case SEL(t,f) =>
            rets ::= codePtr
            codePtr = if (fromNum(pop) == 0) f else t
            return
          case JOIN => rets match {
            case x :: rest => 
              codePtr = x
              rets = rest
            case Nil => error("No return address")
          }
          case LDF(addr) =>
            push(Clos(addr,env))
          case AP(argc) =>
            val Clos(addr,e) = pop
            val args = List.fill(argc)(pop)
            envs ::= env
            rets ::= codePtr
            env = Env(e, args)
            codePtr = addr
            return
          case RTN =>
            env = envs.head
            envs = envs.tail
            codePtr = rets.head
            rets = rets.tail
          case STOP =>
            codePtr = -2
            env = null
        }
        codePtr += 1
      }
  }  

  object Exprs {
    
    case class Cs(cs: Seq[Command]) extends Expr
    case class Un(op: UnaryCommand, arg: Expr) extends Expr
    case class Bi(op: BinaryCommand, left: Expr, right: Expr) extends Expr
    case class If(cond: Expr, t: Expr, f: Expr) extends Expr
    
    implicit def command(c: Command): Expr = Cs(List(c))
    implicit def const(value: Int): Expr = LDC(value)

    trait Expr {
      def +  (that: Expr) = Bi(ADD, this, that)
      def -  (that: Expr) = Bi(SUB, this, that)
      def *  (that: Expr) = Bi(MUL, this, that)
      def == (that: Expr) = Bi(CEQ, this, that)
      def >  (that: Expr) = Bi(CGT, this, that)
      def >= (that: Expr) = Bi(CGTE, this, that)
      def unary_! = Bi(CEQ, this, 0)
      def <  (that: Expr) = ! (this >= that)
      def <= (that: Expr) = ! (this > that)
    }
  }
  import Exprs._
  
  case class Translator(
      commands: Queue[Command] = Queue(),
      blocks:   Vector[Seq[Command]] = Vector()) {
    
    def + (e: Expr): Translator = e match {
      case Cs(cs) => Translator(commands enqueue cs)
      case Un(op, arg) =>
        this + arg + op
      case Bi(op, a, b) =>
        this + b + a + op
      case If(cond, t, f) =>
        (this + cond +@ t +@ f) use (t,f) { SEL(_,_) }
    }
    
    def +@ (e: Expr): Translator = error("Not implemented")
    
    def use(a: (Expr, Expr))(f: (Int,Int) => Command): Translator = error("Not implemented")
  }
  
  def execute(cs: List[Command]): Values = {
    val m = new Machine(cs)
    while (m.env != null) {
      m.next()
    }
    m.data
  }
  
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
//  println(example mkString "\n")
//  print(execute(example))
  import Exprs._
  val x = const(21)
  val example2 = x + x
  println(example2)
  val example2_cd = example2.toCode
  println(example2_cd mkString "\n")
  println(execute(example2_cd ++ List(RTN)))  
}



