package gcc

import Expressions._

class Translator {

  import Translator._
  
  case class Block(id: Int, cmds: Seq[Command])

  private var nextId = START
  
  private var blocks = Map[Expr, Block]()
  private var callStack = Seq[Int]()

  private def block(e: Expr, end: Command): Int =
    blocks get e map (_.id) getOrElse {
      val id = -nextId ; nextId += 1
      val block = compile(e) :+ end
      blocks += e -> Block(id, block) 
      id
    }
  
  private def branch (e: Expr) = block(e, JOIN)

  def closure(e: Expr) = {
    val id = -nextId
    // store current closure id
    // if closure was already compiled, then nothing will be changed
    callStack +:= id 
    var rid = block(e, RTN)
    callStack = callStack.tail
    rid
  }

  private def compile(e: Expr): Seq[Command] = e match {
    case e: Wrapper => 
      Seq( e.command )
      
    case Unary(op, arg) =>
      compile(arg) :+ op
      
    case Binary(op, a, b) =>
      compile(b) ++ compile(a) :+ op
      
    case If(cond, t, f) =>
      compile(cond) :+ SEL(branch(t), branch(f))
      
    case Fun(_, body) =>
      Seq( LDF(closure(body)) )
      
    case Call(f, args) =>
      (args map compile).reverse.flatten ++ compile(f) :+ AP(args.size)
      
    case Recursive =>
      Seq( LDF(callStack.head) )
  }

  def translate(prefix: Command*) = {
    var addresses = Map[Int, Int]()
    
    def tr(addr: Int) =
      if (addr >= 0) addr
      else addresses(addr)
    
    val cmds = blocks.values.foldLeft(prefix) {
      case (left, Block(addr, block)) =>
        val current = left.size
        addresses += addr -> current
        left ++ block
    } map {
      case SEL(t, f) => SEL(tr(t), tr(f))
      case LDF(a) => LDF(tr(a))
      case cmd => cmd
    }
    blocks = blocks mapValues { 
      case Block(addr,block) => Block(tr(addr), block) 
    }
    cmds
  }
  
  def blockOffsets = blocks.toList map {case(e,b) => (b.id,e)} sortBy {_._1}
  
  def printCode(code: Seq[Command]) {
    var i = 0
    var offsets = blockOffsets
    code foreach { x =>
      if (offsets.headOption map {_._1 == i} getOrElse false) {
        println(f"$i%02d: $x ; ${offsets.head._2}")
        offsets = offsets.tail
      }
      else
        println(f"$i%02d: $x")

      i += 1
    }
  }
}

object Translator {
  val START = 2
  var debug = false
  
  def compile(main: Expr, argc: Int) = {
    val t = new Translator
    val res = t.translate(LDF(t closure main), AP(argc), RTN)
    if (debug)
      t.printCode(res)
    res
  }
  def compile(f: Fun): Seq[Command] = compile(f.body, f.argc)
}
