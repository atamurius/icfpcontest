package gcc

import Value._
import Commands._

class Machine(val code: Seq[Command], var data: Values = Nil) {

  private var codePtr = 0

  private var env = Env(null, Nil)

  private var envs: List[Env] = List(null)

  private var rets: List[Int] = List(-2)

  private def push(value: Value) =
    data +:= value
  
  private def pop = data match {
    case x :: rest =>
      data = rest; x
    case Nil => error("No data")
  }

  def next(): Unit = {
    code(codePtr) match {
      case LDC(value) =>
        push(value)
      case LD(fr, ind) =>
        push(env(fr, ind))
      case command: UnaryCommand =>
        push(command(pop))
      case command: BinaryCommand =>
        push(command(pop, pop))
      case SEL(t, f) =>
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
        push(Clos(addr, env))
      case AP(argc) =>
        val Clos(addr, e) = pop
        val args = List.fill(argc)(pop)
        envs ::= env
        rets ::= codePtr
        env = Env(e, args)
        codePtr = addr
        return
      case RTN if !rets.isEmpty =>
        env = envs.head
        envs = envs.tail
        codePtr = rets.head
        rets = rets.tail
      case RTN if rets.isEmpty =>
        codePtr = -2
        env = null
    }
    codePtr += 1
  }

  def printState {
    println(s"executing $codePtr: ${code lift codePtr}")
    println(data.mkString("DATA[", ",", "]"))
    println(s"ENV: $env")
  }
}

object Machine {
  def execute(code: Seq[Command], args: Value*) = {
    val m = new Machine(code, Seq(args: _*))
    try {
      while (m.env != null)
        m.next
      m.data
    } catch {
      case e: Exception =>
        m.printState
        throw e
    }
  }
}
