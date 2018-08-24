package brainfuck

sealed trait Instruction

object Instruction {
  case object IncrementPointer                            extends Instruction
  case object DecrementPointer                            extends Instruction
  case object IncrementCurrentRegister                    extends Instruction
  case object DecrementCurrentRegister                    extends Instruction
  case object OutputCurrentRegister                       extends Instruction
  case object ReadAndSetCurrentRegister                   extends Instruction
  case class DoWhileCurrentRegisterNonZero(loop: Program) extends Instruction
}

final case class Program(value: List[Instruction]) extends AnyVal
