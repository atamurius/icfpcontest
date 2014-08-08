package gcc

import Value._

/**
 * Executes seq of commands step by step
 * @param data initial state of data stack
 */
class Machine(val code: Seq[Command], var data: Values = Nil) {

  private var codePtr = 0

  private var env: Env = EmptyEnv

  private var envStack: Seq[Env] = Nil

  private var returnStack: Seq[Int] = Seq(Machine.HALT)

  private def push(value: Value) = data +:= value
  
  private def pop = data match {
    case x :: rest => data = rest; x
    case Nil       => error("No data")
  }

  private def jump(addr: Int) {
    returnStack +:= codePtr
    codePtr = addr
  }
  
  private def ret = returnStack match {
    case addr :: others => returnStack = others; addr
    case Nil => error("Return stack is empty")
  } 
  
  def isTerminated = (codePtr < 0)
  
  def next(): Unit = {
    if (isTerminated)
      error("Machine is terminated")
    else if (codePtr >= code.size)
      error("End of commands reached")
    else code(codePtr) match {

      // load constant
      case LDC(value) =>
        push(value)
      
      // load from environment
      case LD(fr, ind) =>
        push(env(fr, ind))
        
      // unary command
      case command: UnaryCommand =>
        push(command(pop))
        
      // binary command, reverse order of args
      case command: BinaryCommand =>
        push(command(pop, pop))
        
      // conditional branch
      case SEL(t, f) =>
        return jump(if (pop) t else f)
        
      // return from branch
      case JOIN => 
        codePtr = ret
        
      // create closure
      case LDF(addr) =>
        push(Clos(addr, env))
        
      // execute closure, reverse order or args
      case AP(argc) => pop match {
        case Clos(addr, e) =>
          envStack +:= env
          env = e extend Seq.fill(argc){ pop }
          return jump(addr)
        case x => error(s"Closure expected, but $x found")
      }
      
      // return from closure or program
      case RTN =>
        codePtr = ret
        if (codePtr != Machine.HALT) {
          env  = envStack.head
          envStack = envStack.tail
        }
    }
    codePtr += 1
  }

  def printState {
    println("-------------------------------------")
    println(s"executing $codePtr: ${code lift codePtr}")
    println(data.mkString("DATA[", ",", "]"))
    println(s"ENV: $env")
  }
}

object Machine {
  private val HALT = -2

  var debug = false
  
  def execute(code: Seq[Command], args: Value*) = {
    val m = new Machine(code, Seq(args: _*))
    try {
      while (! m.isTerminated) {
        if (debug)
          m.printState
        m.next
      }
      if (debug)
        m.printState
      m.data
    } catch {
      case e: Exception =>
        m.printState
        throw e
    }
  }
}
