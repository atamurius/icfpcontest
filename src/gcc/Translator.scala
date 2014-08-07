package gcc

import Expressions._
import Commands._

class Translator {

  case class Block(addr: Int, cmds: Seq[Command])

  private var blocks = Map[Expr, Block]()

  def block(e: Expr): Int =
    if (blocks contains e) blocks(e).addr
    else {
      val block = compile(e) :+ RTN
      val addr = -1 - blocks.size
      blocks += e -> Block(addr, block)
      addr
    }

  private def compile(e: Expr): Seq[Command] = e match {
    case Cs(cs) => cs
    case Un(op, arg) =>
      compile(arg) :+ op
    case Bi(op, a, b) =>
      compile(b) ++ compile(a) :+ op
    case If(cond, t, f) =>
      compile(cond) :+ SEL(block(t), block(f))
    case Ap(f, args) =>
      (args map compile).reverse.flatten :+ LDF(block(f)) :+ AP(args.size)
  }

  def translate(prefix: Command*) = {
    var addresses = Map[Int, Int]()
    def tr(addr: Int) =
      if (addr >= 0) addr
      else addresses(addr)
    blocks.values.foldLeft(prefix) {
      case (left, Block(addr, block)) =>
        addresses += addr -> left.size
        left ++ block
    } map {
      case SEL(t, f) => SEL(tr(t), tr(f))
      case LDF(a) => LDF(tr(a))
      case cmd => cmd
    }
  }
}

object Translator {
  def compile(main: Expr, argc: Int) = {
    val t = new Translator
    t.translate(LDF(t block main), AP(argc), RTN)
  }
  def compile(f: Fun): Seq[Command] = compile(f.body, f.arity)
}
